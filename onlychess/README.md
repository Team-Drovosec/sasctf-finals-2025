# OnlyChess

OnlyChess is a service that allows you to play chess (similar to lichess or chess.com). Let's analyze what functionality it has to offer.

![The unauthorized UI view](/images/oc_ui.png)

So far you can:
1. Login/Register
2. Create lobbies
3. Invite a specific player to the game

What is unusual is that all of the functionality could be accessed without registration from an anonymous account.

Let's dive further. When you start a game, the game is added to the "recent games" list that is accessible from the top of the page

![Recent games](/images/oc_recent_games.png)

When you click on a game, it loads the game page.

![Chess game page](/images/oc_game.png)

On the game page you can see the rest of the functionality:

1. Game chat (private, limited to the players)
2. Spectator chat (public)
3. Actual chess board on which you can play

Chats are implemented using websockets, private chats are encrypted. To recover the original messages from ciphertext, you would need to use the key accessible at /chat-key handler (which will check your access rights for that chat). Meanwhile websocket does not check if you're authorized to access it, effectively making it a decoupled microservice that just stores and serves messages, laying the rest of the security on cryptographic part of the service.

## Vulnerability 1: Game access takeover during the deanonimization process

While checking the login handler, you could consider this part of the process a little bit unusual:

```go
recentGames, _ := s.recentGames.GetGameIDs(ctx, *anonUser)
gamesPlayers := s.game.GetPlGetPlayersayers(ctx, recentGames)
for game, gamePlayers := range gamesPlayers {
    for i := range len(gamePlayers) {
        if model.Equal(&gamePlayers[i], anonUser) {
            err = s.game.Deanonimize(ctx, game, *newUser, i%2 == 0)
            if err != nil {
                slog.Error("error deanon game", "err", err)
            }
            err = s.recentGames.AddGame(ctx, *newUser, game)
            if err != nil {
                slog.Error("error add deanon game", "err", err)
            }
            break
        }
    }
}
```

It handles a very useful case:
1. New user plays a game anonymously
2. He then registers and logs in
3. The game that the user has previously played will be transfered to the newly created account


Let's look at the function `s.game.GetPlayers(ctx, recentGames)`
```go
func (r *repo) GetPlayers(ctx context.Context, IDs []string) map[string][]model.User {
	result := make(map[string][]model.User)

	rows, err := r.conn.Query(ctx,
		`SELECT id, "from", "to" FROM game WHERE id = ANY($1)`,
		IDs,
	)
	if err != nil {
		return result
	}
	defer rows.Close()

	for rows.Next() {
		var (
			id, fromStr, toStr string
		)
		if err := rows.Scan(&id, &fromStr, &toStr); err != nil {
			continue
		}

		result[id] = make([]model.User, 2)
		fromUser, err := model.FromString(fromStr)
		if err != nil {
			continue
		}
		result[id] = append(result[id], *fromUser)

		toUser, err := model.FromString(toStr)
		if err != nil {
			continue
		}
		result[id] = append(result[id], *toUser)
	}

	return result
}
```

The issue here is in how the array `result[id]` is initialized. `make([]model.User, 2)` creates an array with two zero elements of model.User. Then it appends real users to this array, resulting in an object like that:
```go
[]model.User{
    {Username: "", AnonymousID: 0},
    {Username: "", AnonymousID: 0},
    user1,
    user2,
}
```

If we look at the `Equal` function for the User type, we can see that any anonymous user would be equal to the "zero user":

```go
func Equal(a, b *User) bool {
    // anonymous id of zero user = 0
	if a.AnonymousID != 0 {
		return a.AnonymousID == b.AnonymousID
	}

    // zero user username is "", and the anonymous user username is also ""
	return a.Username == b.Username
}
```

Another crucial observation is that we can get a game in our "recent games" list by sending a message to the spectator char. On the frontend you can't write to the spectator chat while not authenticated, but the backend does not check for it.
```go
// chat.go
	s.recentGames.AddGame(ctx, *getUser(c), gameID)
```

So, we may try to take over the game of any two users by deanonymizing their game, after which we will be able to get the decryption key and decrypt the flags from the private chat.

- Exploit steps:
	1. Open the target game from an anonymous account
	2. Write something in the spectator chat to get this game in your "recent games" list
	3. Register and login to the new account
	4. Get the private key and decrypt the private chat of target game


#### Golang slices

In Golang, there are two function signatures to make an array:

```go
make([]XXX, 2) // creates the array of length two with two zero elements
make([]XXX, 0, 2) // creates the array of length 0 with capacity for two elements
```

The initialization with the capacity is introduced to increase the performance in cases like:

```go
// BAD:
x = make([]int, 0)

for i := range 100 {
    x = append(x, i)
}
// While appending, the underlying array of x will be copied several times when it would need to increase the capacity

// GOOD:
x = make([]int, 0, 100) // create array of correct capacity initially

for i := range 100 {
    x = append(x, i)
}

// No copies of the array will be made
```

The error in the service code is that the author did write `make([]int, 2)` instead of `make([]int, 0, 2)` - which is a common mistake in Golang.


## Vulnerability 2: Key recovery attack on the crypto algorithm

For the private game chat the service uses a custom stream cipher based on the LILI‑128 design.  
On each `/private-chat` request it:

- fetches a **per‑game 128‑bit key** `key := s.game.GetKey(ctx, game.ID)`,
- generates a fresh **128‑bit IV** `iv := lfsr.KeyGen()`,
- encrypts the message and broadcasts a websocket message with `IsPrivate = true`, the raw `IV` and the base64‑encoded ciphertext to everyone subscribed to `/chat?id=<game>`.

### Mathematical description of the encryption algorithm

On a high level the service implements the same structure as LILI‑128.  
We work over $GF(2)$, so all additions are XORs. The per‑game secret key is
$s \in \\{0,1\\}^{128}$, and for each message the server samples a fresh
$iv \in \\{0,1\\}^{128}$.

The key and IV are mixed and loaded into two LFSRs:

- the first 39 bits initialize register $C$,
- the remaining 89 bits initialize register $D$,

so that effectively $C$ and $D$ start from the state corresponding to $s \oplus iv$.  
At each step the cipher does the following:

1. Clock the C‑register once with a fixed linear feedback and compute a small integer
   $c_t = 2 C_{12} + C_{20} + 1 \in \\{1,2,3,4\\}$, which plays the role of a **clocking value**.
2. Using the current state of the D‑register, evaluate a non‑linear filter function $f_D$ that looks at several taps of $D$ and a fixed lookup table. The result is the keystream bit $z_t \in \\{0,1\\}$.
3. Clock the D‑register $c_t$ times with its own linear feedback.
4. XOR the keystream bit with the plaintext bit:
   $c'_t = m_t \oplus z_t$.

If we denote by $LILI_{128,k}(n)$ the keystream of length $n$ produced by this construction with key $k$, then the server is effectively using
$k_{\text{eff}} = s \oplus iv$, so for an $n$‑bit message it computes
$\text{keystream} = LILI_{128,k_{\text{eff}}}(n)$ and $\text{ciphertext} = m \oplus \text{keystream}$,
and returns $\text{ciphertext}$ together with the public $iv$. Decryption is symmetric because XOR is its own inverse.

This is exactly the "key‑whitening with public IV" construction that was proved insecure in Babbage's cryptanalysis of LILI‑128.  
If an attacker can obtain many pairs $(IV_i, \text{keystream}_i)$ generated with the same secret key and different IVs, the original 128‑bit key can be recovered significantly faster than brute force by exploiting the structure of the two LFSRs.

### Attack description

Assume the attacker can query the encryption oracle many times and observes pairs $(IV_i, c_i)$, where each ciphertext $c_i$ encrypts a **known plaintext** (for example, a fixed ASCII string) under the same secret key but a different IV.  
From each such pair the attacker can reconstruct the corresponding keystream prefix as $\text{keystream}_i = m \oplus c_i$. Thus the attacker effectively knows a set of pairs $(IV_i, \text{keystream}_i)$, which is exactly the setting considered in [2].

**Phase 1 (recovering the 39 bits of the C‑register).**  
The irregular clocking value $c_t = 2 C_{12} + C_{20} + 1$ depends on two bits of the C‑register.  
For different IVs the initial C‑state is $C^{(0)} = C^{(0)}(s, iv)$, but the structure of the feedback is fixed. Babbage's idea is the following:

- Fix a hypothesis about the two bits of the C‑register that control $\(c_t\)$ at a certain time.
- For many pairs of IVs, track how far the C‑register runs and find positions where the corresponding D‑register input to the nonlinear function $\(f_D\)$ should be the same for both IVs.
- If the guessed C‑bits are correct, the keystream bits at those positions must coincide for both IVs; if they differ, the hypothesis is wrong and can be discarded.

By repeating this procedure and sliding along the keystream, we iteratively recover pairs of bits of the C‑register until all 39 bits are known (up to solving a small system of linear equations that arises from overlaps between steps).

**Phase 2 (recovering the 89 bits of the D‑register).**  
Once the C‑register is known, the sequence of clocking values $c_t$ is fully determined for any IV. The D‑register itself is linear, and its state at time $t$ can be expressed as a linear function over GF(2) of:

- the 89 unknown key bits that initialize $D$,
- the public bits of the IV.

The nonlinear filter $f_D$ takes some taps of this state and outputs a keystream bit that we already know from the oracle. For a fixed time $t$ and a fixed IV, the relation
$f_D(D^{(t)}(s, iv)) = keystream_i[t]$
can be expanded into a (generally nonlinear) equation in the 89 unknown key bits. The trick from [2] is to choose those time instants and IVs for which the input to $f_D$ depends only on a small subset of the key bits; for these positions $f_D$ can be "inverted" by brute force on 10 input variables, and each successful inversion gives a **linear equation** on the underlying 89 key bits.

Collecting enough such equations from many $(IV_i, \text{keystream}_i)$ pairs yields an overdetermined linear system over GF(2) with 89 unknowns. Solving it gives the full initial state of the D‑register and thus the remaining 89 bits of the secret key.

The CTF infrastructure exposes exactly the oracle required for this attack:

- Anyone can subscribe to the websocket `/chat?id=<game>` and passively receive all private messages, including their `IV` and base64‑encoded ciphertext, even without being a player of the game.
- The `/private-chat` HTTP handler lets any user (including an anonymous one) send arbitrary plaintext for any `game` ID; each request creates a new random IV and an encrypted private message that is immediately broadcast over the websocket.

By repeatedly sending a known plaintext (for example, `"A" * 16` bytes) via `/private-chat` for a chosen game and listening on the websocket, we obtain many samples of $(IV, \text{ciphertext})$.  
Since the plaintext is known, this directly gives us $(IV, \text{keystream})$ pairs, and the Python script [exploit/crypto_exploit.py](exploit/crypto_exploit.py) implements both phases of Babbage's attack to recover the full 128‑bit game key.

Once the game key is recovered, the attacker does not need `/chat-key` at all: they can locally instantiate the same cipher and decrypt every past and future private message for that game using the captured `(IV, ciphertext)` pairs.

