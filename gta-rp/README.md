# gta-rp (Gold, Tools and Armor: RolePlay)

## Overview

- Services
  - 25565 — Minecraft server (PaperMC 1.21.8)
  - 25566 — [Yggdrasil server](https://wiki.vg/Legacy_Mojang_Authentication): a simplified Minecraft authentication server

- Gameplay
  - Register on the Yggdrasil server.
  - Choose your role:
    - Miner
    - Business owner
  - After registration you receive a TOTP secret. TOTP's are used instead of a regular password.
  - Business owners manage mines and can change their settings.
  - Miners work in owners' mines and can furnish their apartments.
    - Earn XP for each mined block.
    - Spend XP to:
      - receive salary (in the blocks/items chosen by the owner)
      - upgrade your grade level in the current job
    - You can also change your workplace if you want to.
  - Each grade level in a mine contains owner-defined text.
- Flags are stored in the text tied to the maximum grade level (5), which is impossible to earn during the flag's validity lifespan.


## Vulnerabilities
### 1. Weak PRNG in TOTP secret generation
Yggdrasil server is implemented in Haskell. Let's dive deeper in how TOTP secrets are generated:

```haskell
generateRandomOTPSecret = do
  a <- getRandom
  b <- getRandom
  let ab = toLazyByteString (word64BE a <> word64BE b)
      secret = B32.encodeBase32Unpadded (LBS.toStrict ab)
  return secret
```

What's interesting, is that random generator is initialized only once at the application level: `stdGen <- newIORef =<< newStdGen`. This means each subsequent value derives from the same initial seed, so it may be possible to recover generator's current internal state from a few consecutive outputs and thereby predict past and future generated values. Haskell internally uses an implementation of SplitMix PRNG, whose state can be recovered from just two consecutive outputs. Conveniently, a single TOTP secret is constructed from exactly two outputs, meaning that we don't need to race with other users of this service.

- Exploit steps:
  1. Register a new user.
  2. Recover the SplitMix state from your TOTP secret.
  3. Use backwards-predicted secrets to generate TOTP codes for the checkers' accounts (from the attack data).
  4. If a login succeeds, join the server.
  5. Open the business owner menu and read the flag.

Check [random_exploit.py](./exploit/auth-random/random_exploit.py) for the code.

### 2. Inventory GUI spoofing via renamed chest
GTARP plugin inventory GUI uses inventory title for state control without any proper access checks. In Minecraft you can rename items using an anvil. By renaming a chest to `Mine {username}` you can get into the owner menu for a target mine.

- Exploit steps:
  1. Register a business owner and a miner.
  2. Set iron as the salary item.
  3. Mine and collect salary.
  4. Change the salary item to wood.
  5. Mine again and collect salary.
  6. Go to `/home`.
  7. Craft a Crafting Table, an Anvil, and a Chest.
  8. Rename the chest to the target menu title - `Mine {username}`.
  9. Place the chest and open it.
  10. Click any slot to trigger a menu redraw.
  11. You now have access to the mine settings and can read the flags.

### 3. Broken logic in worker rewards
Business owners can reward workers with an extra XP. As it is implied by the code, after the reward menu is rendered, XP can be granted with a single click without further checks (like if this miner is still employed at your business). An SQL update is broad and applies to all user's *active* employments, assuming that only the current job might be active at the time of this update:

```sql
UPDATE worker_employments
SET experience = experience + $2
WHERE worker_name = $1 AND is_active = TRUE
RETURNING experience, is_active
```

- Exploit steps:
  1. Register a business owner and a miner.
  2. Get your miner employed at the mine controlled by your business owner.
  3. Open the reward menu.
  4. Switch the miner's job to the checker's mine.
  5. Click reward. The miner receives XP for the checker's mine.
  6. Upgrade to the maximum level and read the flag.


## Automation and Local Testing
- The easiest way to automate attacks is to use Minecraft scripting libraries, eg. **mineflayer**
- For local testing, use a launcher that supports `authlib-injector`, eg. **Fjord** or **HMCL**
