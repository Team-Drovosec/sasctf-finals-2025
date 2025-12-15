import { argv, env, fetch, sleep } from "bun";
import mineflayer, { Chest, Dispenser, type Bot } from "mineflayer";
import { pathfinder, goals, Movements, type Move } from "mineflayer-pathfinder";
import type { Window } from "prismarine-windows";
import {type Entity} from "minecraft-data";
import mcproto from "minecraft-protocol";
import { createTask, once, withTimeout } from "./tricks";
import { randomBytes, randomInt } from "node:crypto";
import { Item } from "prismarine-item";
import { Block } from "prismarine-block";
import { Vec3 } from "vec3";
import { v4 } from "uuid";
import { appendFileSync, writeFileSync } from "node:fs";
import { TOTP } from "totp-generator";
import * as z from "zod";
import UserAgent from "user-agents";
import { sl } from "zod/locales";

// Menu slot enums
enum OwnerMainMenuSlots {
    MANAGING_NAME_TAG = 10,
    SALARY_ITEM = 12,
    XP_PER_BLOCK = 14,
    MANAGE_WORKERS = 16,
    LEVEL_0_MESSAGE = 28,
    LEVEL_1_MESSAGE = 29,
    LEVEL_2_MESSAGE = 30,
    LEVEL_3_MESSAGE = 31,
    LEVEL_4_MESSAGE = 32,
    LEVEL_5_MESSAGE = 33,
    CLOSE_MENU = 44,
}

enum OwnerSalarySelectionSlots {
    COAL = 0,
    IRON_INGOT = 1,
    GOLD_INGOT = 2,
    EMERALD = 3,
    DIAMOND = 4,
    BREAD = 5,
    COBBLESTONE = 6,
    OAK_LOG = 7,
    COPPER_INGOT = 8,
    BACK = 26,
}

enum OwnerMinersMenuSlots {
    WORKER_START = 0,
    WORKER_END = 36,
    PREVIOUS_PAGE = 38,
    PAGE_NUMBER = 40,
    NEXT_PAGE = 42,
    BACK = 44,
}

enum MinerJobsMenuSlots {
    JOB_LIST_START = 0,
    JOB_LIST_END = 20,
    PREVIOUS_PAGE = 21,
    PAGE_NUMBER = 22,
    NEXT_PAGE = 23,
    LEVEL = 24,
    SALARY = 25,
    CLOSE_MENU = 26,
}

function Parse<T>(schema: z.ZodType<T>, input: unknown): T {
    const r = schema.safeParse(input, { reportInput: true });
    if (!r.success) {
        throw new MumbleError(`${Bun.inspect(r.error)}`, "");
    }
    return r.data;
}

namespace yggdrasil {
    interface LoginWithSecretOption {
        username: string;
        secret: string;
    }

    const headers = {
        "User-Agent": "Java/21.0.8",
        "Content-Type": "application/json",
    };
    function urlForAction(address: string, action: string): string {
        return `http://${address}:25566/authserver/${action}`;
    }

    export async function loginWithOtp(
        address: string,
        option: LoginWithSecretOption,
    ): Promise<{accessToken: string, clientToken: string, selectedProfile: {id: string, name: string}}> {
        const clientToken = v4();
        let otpCode = await TOTP.generate(option.secret, {
            digits: 8,
            explicitZeroPad: true,
        });
        if (otpCode.expires - Date.now() < 3000) {
            await sleep(3000);
            otpCode = await TOTP.generate(option.secret, {
                digits: 8,
                explicitZeroPad: true,
            });
        }
        const resp = await fetchRejectToDown(
            urlForAction(address, "authenticate"),
            {
                method: "POST",
                headers,
                body: JSON.stringify({
                    agent: {
                        name: "Minecraft",
                        version: 1,
                    },
                    username: option.username,
                    password: otpCode.otp,
                    clientToken,
                    requestUser: false,
                }),
            },
        );
        const ResponseData = z.object({
            accessToken: z.string(),
            clientToken: z.string(),
            selectedProfile: z.object({
                id: z.string(),
                name: z.string(),
            }),
        });
        try {
            if (resp.status !== 200) {
                throw new MumbleError(
                    `Authenticate for ${JSON.stringify(option)} returned ${resp.status} with ${await resp.text()}`,
                    "",
                );
            }
            const data = Parse(ResponseData, await resp.json());
            if (data.clientToken !== clientToken) {
                throw new MumbleError(
                    `Server returned unexpected clientToken ${data.clientToken} !== ${clientToken}`,
                    "",
                );
            }
            writeToLog(`loginOTP ${Bun.inspect(data)}`)
            return {
                accessToken: data.accessToken,
                clientToken: data.clientToken,
                selectedProfile: {
                    id: data.selectedProfile.id,
                    name: data.selectedProfile.name,
                },
            };
        } catch (err) {
            if (err instanceof MumbleError) {
                err.checkerText = "error in /authenticate";
                throw err;
            } else {
                throw new MumbleError(
                    `Can't login: ${resp.status} ${resp.url} ${Bun.inspect(err)}`,
                    "error in /authenticate",
                );
            }
        }
    }

    interface ValidateOption {
        accessToken: string;
        clientToken: string;
        expectSuccess: boolean;
    }

    export async function validate(
        address: string,
        option: ValidateOption,
    ): Promise<void> {
        const resp = await fetchRejectToDown(
            urlForAction(address, "validate"),
            {
                method: "POST",
                headers,
                body: JSON.stringify({
                    accessToken: option.accessToken,
                    clientToken: option.clientToken,
                }),
            },
        );
        if (resp.status !== 204 && option.expectSuccess) {
            throw new MumbleError(
                `We expected valid, but server said it's invalid session ${resp.status} with ${await resp.text()}`,
                "error in session validate",
            );
        } else if (resp.status === 204 && !option.expectSuccess) {
            throw new MumbleError(
                `We expected invalid, but server said it's valid session ${resp.status} with ${await resp.text()}`,
                "error in session validate",
            );
        }
        writeToLog(`validate ${Bun.inspect(option)}`)
    }

    export async function invalidate(
        address: string,
        option: ValidateOption,
    ): Promise<void> {
        const resp = await fetchRejectToDown(
            urlForAction(address, "invalidate"),
            {
                method: "POST",
                headers,
                body: JSON.stringify({
                    accessToken: option.accessToken,
                    clientToken: option.clientToken,
                }),
            },
        );
        if (resp.status !== 204 && option.expectSuccess) {
            throw new MumbleError(
                `We expected successful invalidation, but server returned ${resp.status} with ${await resp.text()}`,
                "error in session invalidate",
            );
        } else if (resp.status === 204 && !option.expectSuccess) {
            throw new MumbleError(
                `We expected error invalidation, but server returned ${resp.status} with ${await resp.text()}`,
                "error in session invalidate",
            );
        }
        writeToLog(`invalidate ${Bun.inspect(option)}`)
    }
}

class MumbleError extends Error {
    checkerText: string;
    window: Window | undefined;

    constructor(message: string, checkerText: string, window: Window|undefined=undefined) {
        super(message);
        this.checkerText = checkerText;
        this.window = window;
    }
}

class DownError extends Error {}

function wrapMumbleError<F extends (...args: any[]) => Promise<any>>(
    publicMsg: string,
    _fn: F,
): never;
function wrapMumbleError<F extends (...args: any[]) => any>(
    publicMsg: string,
    fn: F,
): (...args: Parameters<F>) => ReturnType<F>;

function wrapMumbleError(publicMsg: string, fn: (...args: any[]) => any) {
    return function (this: unknown, ...args: any[]) {
        try {
            return fn.apply(this, args);
        } catch (err) {
            if (err instanceof MumbleError) {
                if(err.window != null) {
                    for(const slot of err.window.slots.filter(x=>x)) {
                        const customName = (slot as any)?.componentMap?.get("custom_name") ? Parse(CustomNameRaw, (slot as any)?.componentMap?.get("custom_name")).data : null;
                        const lore = (slot as any)?.componentMap?.get("lore") ? Parse(LoreRaw, (slot as any)?.componentMap?.get("lore")).data : null;
                        writeToLog(`window.slots[${slot?.slot}], "${customName}", ${JSON.stringify(lore)}`)
                    }
                    err.window = undefined;
                }
                err.checkerText = publicMsg;
                throw err;
            } else {
                throw new MumbleError(Bun.inspect(err), publicMsg);
            }
        }
    };
}

async function wrapRejectToDown<T>(promise: Promise<T>): Promise<T> {
    try {
        return await promise;
    } catch (err) {
        throw new DownError("wrapped down", { cause: err });
    }
}

async function fetchRejectToDown(
    url: string,
    options: BunFetchRequestInit,
): Promise<Response> {
    return await wrapRejectToDown(fetch(url, {...options, signal: AbortSignal.timeout(3000)}));
}

interface FullLoginOptions {
    address: string;
    session: mcproto.SessionOption;
    version: string;
}

interface BotWithLog extends mineflayer.Bot {
    print: (text: string) => void;
    wrap<T>(promise: Promise<T>): Promise<T>;
    home: Vec3;
}

async function joinServer(options: FullLoginOptions): Promise<BotWithLog> {
    const username = options.session.selectedProfile.name;
    const bot = mineflayer.createBot({
        host: options.address,
        fakeHost: options.address,
        username,
        session: options.session,
        sessionServer: `http://${options.address}:25566/sessionserver`,
        auth: "mojang",
        port: 25565,
        version: options.version,
        skipValidation: true,
        // hideErrors: true,
        plugins: {
            chatInject: (bot, options) => {
                const chatOrig = bot.chat;
                bot.chat = (message: string) => {
                    chatOrig(message);
                    (bot as BotWithLog).print(`sent "${message}"`);
                };
            },
        },
    }) as BotWithLog;
    (bot as BotWithLog).print = (text: string) => {
        writeToLog(`[${username}] ${text}`);
    };
    (bot as BotWithLog).wrap = async (promise) => {
        try {
            return await promise;
        } catch (err) {
            bot.print(`${err}`);
            throw err;
        }
    };
    bot.on("message", (msg, position) => {
        bot.print(`msg(${position}): ${msg}`);
        if (
            position === "system" &&
            msg.toString().includes("You were kicked from")
        ) {
            bot.print("Unexpected kick");
            exitProcess("DOWN");
        }
    });
    bot.on("end", async (reason: string) => {
        bot.print(`end: ${reason}`);
        if (reason !== "disconnect.quitting") {
            bot.print("Unexpected disconnect");
            exitProcess("DOWN");
        }
    });
    bot.on("error", (error: Error) => {
        bot.print(`error: ${error}`);
    });
    bot.on("login", () => {
        bot.print("login");
    });
    bot.on("kicked", (reason: string) => {
        bot.print(`kicked: ${reason}`);
    });
    bot.loadPlugin(pathfinder);

    await bot.wrap(once(bot, "spawn") as unknown as Promise<void>);
    const movements = new Movements(bot);
    movements.canDig = false;
    bot.pathfinder.setMovements(movements);
    bot.print(`Spawned at ${bot.entity.position}`);
    await bot.wrap(bot.waitForChunksToLoad());
    return bot;
}

interface UserData {
    username: string;
    role: "miner" | "businessman";
}

interface RegisteredUserData extends UserData {
    secret: string;
}

const alphabet = "abcdefghijklmnopqrstuvwxyz0123456789";
function randomString(n: number): string {
    return [...Array(n).keys()]
        .map(() => alphabet[randomInt(alphabet.length)])
        .join("");
}

async function registerUser(
    addr: string,
    userData: UserData,
): Promise<RegisteredUserData> {
    writeToLog(`registering ${userData.role} ${userData.username}`);
    const url = `http://${addr}:25566`;
    const browserUA = new UserAgent({ deviceCategory: "desktop" }).toString();
    await fetchRejectToDown(url, {
        method: "GET",
        headers: {
            "User-Agent": browserUA,
        },
    });
    const ResponseData = z.object({
        otpSecret: z.string().length(26),
    });
    const resp = await fetchRejectToDown(`${url}/api/v1/register`, {
        method: "POST",
        headers: {
            "User-Agent": browserUA,
            "Content-Type": "application/json",
        },
        body: JSON.stringify({
            username: userData.username,
            userClass: userData.role,
        }),
    });
    try {
        if (resp.status !== 200) {
            throw new MumbleError(
                `Can't register: ${resp.status} ${resp.url} ${await resp.text()}`,
                "",
            );
        }
        return {
            ...userData,
            secret: Parse(ResponseData, await resp.json()).otpSecret,
        };
    } catch (err) {
        if (err instanceof MumbleError) {
            err.checkerText = "Can't register";
            throw err;
        } else {
            throw new MumbleError(
                `Can't register: ${resp.status} ${resp.url}`,
                "Can't register",
            );
        }
    }
}

type StatusChecker = "OK" | "CORRUPT" | "MUMBLE" | "DOWN" | "CHECKER_ERROR";

function exitProcess(status: StatusChecker): never {
    writeToLog(`Ending with ${status}`);
    switch (status) {
        case "OK":
            process.exit(101);
        case "CORRUPT":
            process.exit(102);
        case "MUMBLE":
            process.exit(103);
        case "DOWN":
            process.exit(104);
        case "CHECKER_ERROR":
            process.exit(110);
    }
}

function logPlayersAround(bot: BotWithLog) {
    const radius = 10;
    const playerNames = Object.values(bot.entities)
        .filter(
            (entity) =>
                entity.type === "player" &&
                entity.position.distanceTo(bot.entity.position) < radius,
        )
        .map((entity) => entity.username);
    bot.print(
        `Found ${playerNames.length} in radius ${radius}: ${playerNames}`,
    );
}

async function getRandomBot(
    address: string,
    role: "miner" | "businessman",
): Promise<BotWithLog> {
    const userdata: RegisteredUserData = await registerUser(address, {
        username: randomString(8),
        role,
    });
    const session = await yggdrasil.loginWithOtp(address, userdata);
    const bot = await joinServer({
        session,
        address,
        version: "1.21.8",
    });
    bot.home = bot.entity.position.clone();
    bot.digTime = (_) => 1100;
    return bot;
}

const CustomNameRaw = z.object({
    type: z.string().regex(/^custom_name$/),
    data: z
        .object({
            type: z.string().regex(/^string$/),
            value: z.string(),
        })
        .transform((x) => x.value),
});

const LoreRaw = z.object({
    type: z.string().regex(/^lore$/),
    data: z
        .array(
            z.object({
                type: z.string().regex(/^string$/),
                value: z.string(),
            }),
        )
        .transform((x) => x.map((x) => x.value)),
});

function GetSlotInfo(slot: Item | null | undefined) {
    return {
        customName: Parse(CustomNameRaw, (slot as any)?.componentMap?.get("custom_name")).data,
        lore: Parse(LoreRaw, (slot as any)?.componentMap?.get("lore")).data
    }
}

function CheckSlot(
    slot: Item | null | undefined,
    nameExpected: string,
    loreExpected: string[],
) {
    writeToLog(`CheckSlot(${slot?.slot}, "${nameExpected}", ${JSON.stringify(loreExpected)})`);
    const {customName, lore} = GetSlotInfo(slot);
    if (customName !== nameExpected) {
        throw new MumbleError(`${customName} != ${nameExpected}`, "");
    }
    if (
        lore.length !== loreExpected.length ||
        !lore.every((v, i) => v === loreExpected[i])
    ) {
        throw new MumbleError(
            `${JSON.stringify(lore)} != ${JSON.stringify(loreExpected)}`,
            "",
        );
    }
}

const CheckOwnerMainMenu = wrapMumbleError(
    "unexpected owner interface",
    (window: Window, username: string): {salary: string, xp: number, level0: string, level1: string, level2: string, level3: string, level4: string, level5: string} => {
        Parse(
            z.object({
                type: z.string().regex(/^string$/),
                value: z.templateLiteral(["Mine ", username]),
            }),
            window.title,
        );

        CheckSlot(
            window.slots[OwnerMainMenuSlots.MANAGING_NAME_TAG],
            `Managing: ${username}`,
            ["Mine owner:", username],
        );
        const salarySlot = GetSlotInfo(window.slots[OwnerMainMenuSlots.SALARY_ITEM]);
        const salaryRes = /^Payout item: (.*)$/.exec(salarySlot.customName);
        if (salaryRes == null || salarySlot.lore.length !== 1 || salarySlot.lore[0] !== "Click to choose the reward item") {
            throw new MumbleError("", "", window);
        }
        const xpSlot = GetSlotInfo(window.slots[OwnerMainMenuSlots.XP_PER_BLOCK]);
        if(xpSlot.customName !== "XP per block" || xpSlot.lore.length !== 3) {
            throw new MumbleError("", "", window);
        }
        const xpRes = /^Current reward: (\d+)$/.exec(xpSlot.lore[0]!);
        if(xpRes == null || xpSlot.lore[1] !== "Left click: +1, Shift+Left: +10" || xpSlot.lore[2] !== "Right click: -1, Shift+Right: -10") {
            throw new MumbleError("", "", window);
        }
        CheckSlot(
            window.slots[OwnerMainMenuSlots.MANAGE_WORKERS],
            "Manage workers",
            ["Click to view employees", "Grant bonuses directly"],
        );
        const ParseMsg = (item: Item, n: number) => {
            const slotInfo = GetSlotInfo(item);
            if(slotInfo.customName != `Level ${n} message` || slotInfo.lore.length < 4) {
                throw new MumbleError("", "", window);
            }
            if(slotInfo.lore[0] !== "Current text:" ||
                slotInfo.lore[slotInfo.lore.length-1] !== `Click to edit level ${n} message` ||
                slotInfo.lore[slotInfo.lore.length-2] !== " "
            ) {
                throw new MumbleError("", "", window);
            }
            return slotInfo.lore.slice(1, slotInfo.lore.length-2).join(" ");
        };
        const level0 = ParseMsg(window.slots[OwnerMainMenuSlots.LEVEL_0_MESSAGE]!, 0);
        const level1 = ParseMsg(window.slots[OwnerMainMenuSlots.LEVEL_1_MESSAGE]!, 1);
        const level2 = ParseMsg(window.slots[OwnerMainMenuSlots.LEVEL_2_MESSAGE]!, 2);
        const level3 = ParseMsg(window.slots[OwnerMainMenuSlots.LEVEL_3_MESSAGE]!, 3);
        const level4 = ParseMsg(window.slots[OwnerMainMenuSlots.LEVEL_4_MESSAGE]!, 4);
        const level5 = ParseMsg(window.slots[OwnerMainMenuSlots.LEVEL_5_MESSAGE]!, 5);
        
        CheckSlot(window.slots[OwnerMainMenuSlots.CLOSE_MENU], "Close menu", [
            "Return to gameplay",
        ]);

        return {salary: salaryRes[1]!, xp: parseInt(xpRes[1]!), level0, level1, level2, level3, level4, level5};
    },
);

const CheckOwnerSalarySelection = wrapMumbleError(
    "unexpected owner interface",
    (window: Window) => {
        CheckSlot(window.slots[OwnerSalarySelectionSlots.COAL], "COAL", [
            "Click to select this option",
        ]);
        CheckSlot(
            window.slots[OwnerSalarySelectionSlots.IRON_INGOT],
            "IRON_INGOT",
            ["Click to select this option"],
        );
        CheckSlot(
            window.slots[OwnerSalarySelectionSlots.GOLD_INGOT],
            "GOLD_INGOT",
            ["Click to select this option"],
        );
        CheckSlot(window.slots[OwnerSalarySelectionSlots.EMERALD], "EMERALD", [
            "Click to select this option",
        ]);
        CheckSlot(window.slots[OwnerSalarySelectionSlots.DIAMOND], "DIAMOND", [
            "Click to select this option",
        ]);
        CheckSlot(window.slots[OwnerSalarySelectionSlots.BREAD], "BREAD", [
            "Click to select this option",
        ]);
        CheckSlot(
            window.slots[OwnerSalarySelectionSlots.COBBLESTONE],
            "COBBLESTONE",
            ["Click to select this option"],
        );
        CheckSlot(window.slots[OwnerSalarySelectionSlots.OAK_LOG], "OAK_LOG", [
            "Click to select this option",
        ]);
        CheckSlot(
            window.slots[OwnerSalarySelectionSlots.COPPER_INGOT],
            "COPPER_INGOT",
            ["Click to select this option"],
        );
        CheckSlot(window.slots[OwnerSalarySelectionSlots.BACK], "Back", [
            "Return to mine management",
        ]);
    },
);

const CheckOwnerMiners = wrapMumbleError(
    "unexpected owner interface",
    (window: Window) => {
        // CheckSlot(window.slots[13], "No active workers", ["Hire employees to see them here"]);
        CheckSlot(
            window.slots[OwnerMinersMenuSlots.PREVIOUS_PAGE],
            "No previous page",
            ["Already on the first page"],
        );
        const pageNumber = GetSlotInfo(window.slots[OwnerMinersMenuSlots.PAGE_NUMBER]);
        const pageRes = /^Page (\d+) of (\d+)$/.exec(pageNumber.customName);
        if(pageRes == null || pageNumber.lore.length !== 1) {
            throw new MumbleError("", "", window);
        }
        if(pageNumber.lore[0] !== "No workers to display" && /^Showing \d+-\d+ of \d+$/.exec(pageNumber.lore[0]!) == null) {
            throw new MumbleError("", "", window);
        }
        // CheckSlot(window.slots[OwnerMinersMenuSlots.NEXT_PAGE], "No next page", ["Already on the last page"]);
        CheckSlot(
            window.slots[OwnerMinersMenuSlots.BACK],
            "Back to mine menu",
            ["Return to mine management"],
        );
    },
);


const JobList = wrapMumbleError(
    "unexpected owner interface",
    (window: Window) => {
        return window.slots
            .slice(MinerJobsMenuSlots.JOB_LIST_START, MinerJobsMenuSlots.JOB_LIST_END + 1)
            .filter(x => x)
            .map((slot) => {
                const jobInfo = GetSlotInfo(slot);
                const ownerRegexRes = /^Owner: (.*)$/.exec(jobInfo.customName);
                if (ownerRegexRes == null || jobInfo.lore.length !== 3) {
                    throw new MumbleError("", "");
                }
                
                const payoutRegexRes = /^Payout: (.*)$/.exec(jobInfo.lore[0]!);
                const xpRegexRes = /^XP per block: (\d+)$/.exec(jobInfo.lore[1]!);
                if (payoutRegexRes == null || xpRegexRes == null || (jobInfo.lore[2] !== "Click to start working" && jobInfo.lore[2] !== "Click to teleport")) {
                    throw new MumbleError("", "");
                }
                return {active: jobInfo.lore[2] === "Click to teleport", index: slot!.slot!, owner: ownerRegexRes[1]!, salary: payoutRegexRes[1]!, xp: parseInt(xpRegexRes[1]!)};
            });
    }
);

const CheckMinerMenu = wrapMumbleError(
    "unexpected miner interface",
    (window: Window, username: string): {page: number, maxPages: number, jobInfo: {level: number, levelMsg: string, xp: number, salary: string} | null} => {
        const pageTitle = Parse(
            z.object({
                type: z.string().regex(/^string$/),
                value: z.string(),
            }),
            window.title,
        ).value;
        const regexPageRes = /^Jobs (.*) \((\d+)\)$/.exec(pageTitle);
        if(regexPageRes == null || regexPageRes[1] !== username) {
            throw new MumbleError(`${pageTitle}`, "", window);
        }
        const page = parseInt(regexPageRes[2]!);
        const prevPage = GetSlotInfo(window.slots[MinerJobsMenuSlots.PREVIOUS_PAGE]);
        if (page === 1) {
            if(prevPage.customName !== "No previous page" || prevPage.lore.length !== 1 || prevPage.lore[0] !== "You are on the first page") {
                throw new MumbleError("", "", window);
            }
        } else if(prevPage.customName !== "Previous Page" || prevPage.lore.length !== 1 || prevPage.lore[0] !== "View earlier jobs") {
            throw new MumbleError("", "", window);
        }
        const pageNum = GetSlotInfo(window.slots[MinerJobsMenuSlots.PAGE_NUMBER]);
        const pageNumRegexRes = /^Page (\d+) of (\d+)$/.exec(pageNum.customName);
        if (pageNumRegexRes === null) {
            throw new MumbleError("", "", window);
        }
        const pageNumCurrent = parseInt(pageNumRegexRes[1]!);
        const maxPages = parseInt(pageNumRegexRes[2]!);
        if (pageNumCurrent !== page) {
            throw new MumbleError("", "", window);
        }
        if (pageNum.lore.length !== 1) {
            throw new MumbleError("", "", window);
        }
        if (/^Showing \d+-\d+ of \d+$/.test(pageNum.lore[0]!) == null) {
            throw new MumbleError("", "", window);
        }
        const nextPage = GetSlotInfo(window.slots[MinerJobsMenuSlots.NEXT_PAGE]);
        if(page === maxPages) {
            if (nextPage.customName !== "No next page" || nextPage.lore.length !== 1 || nextPage.lore[0] !== "You are on the last page") {
                throw new MumbleError("", "", window);
            }
        } else {
            if (nextPage.customName !== "Next Page" || nextPage.lore.length !== 1 || nextPage.lore[0] !== "View more jobs") {
                throw new MumbleError("", "", window);
            }
        }

        const levelBtn = GetSlotInfo(window.slots[MinerJobsMenuSlots.LEVEL]);
        const salaryBtn = GetSlotInfo(window.slots[MinerJobsMenuSlots.SALARY]);

        let jobInfo: {level: number, levelMsg: string, xp: number, nextLevelCost: number | null, salary: string} | null = null;

        if (levelBtn.customName === "No level available" && levelBtn.lore.length === 1 && levelBtn.lore[0] === "Get a job to unlock upgrades") {
            if (salaryBtn.customName !== "No salary available" || salaryBtn.lore.length !== 1 || salaryBtn.lore[0] !== "Get a job to collect salary") {
                throw new MumbleError("", "", window);
            }
        } else {
            const levelRegexRes = /^Worker level (\d+)$/.exec(levelBtn.customName);
            if (levelRegexRes == null || levelBtn.lore.length < 6) {
                throw new MumbleError("", "", window);
            }
            const levelRegexRes2 = /^Current level: (\d+)$/.exec(levelBtn.lore[0]!);
            if(levelRegexRes2 == null || levelBtn.lore[1] !== " " ) {
                throw new MumbleError("", "", window);
            }
            if (levelRegexRes[1] != levelRegexRes2[1]) {
                throw new MumbleError("", "", window);
            }
            let levelMax = -1;
            let nextLevelCostRes;
            if (levelBtn.lore[levelBtn.lore.length - 1] === "You reached  the maximum level.") {
                levelMax = 3;
                const xpLevelRes = /^Your XP: (\d+)$/.exec(levelBtn.lore[levelBtn.lore.length - 2]!);
                if(xpLevelRes == null) {
                    throw new MumbleError("", "");
                }
            } else {
                levelMax = 5;
                if(levelBtn.lore[levelBtn.lore.length - 1] !== "Click to upgrade" || levelBtn.lore.length < 8) {
                    throw new MumbleError("", "", window);
                }
                nextLevelCostRes = /^Next level cost: (\d+) XP$/.exec(levelBtn.lore[levelBtn.lore.length - 2]!);
                const remainingXpLevelRes = /^Need (\d+) more XP\.$/.exec(levelBtn.lore[levelBtn.lore.length - 3]!);
                const xpLevelRes = /^Your XP: (\d+)$/.exec(levelBtn.lore[levelBtn.lore.length - 4]!);
                
                if (nextLevelCostRes == null || xpLevelRes == null) {
                    throw new MumbleError("", "", window);
                }
                if(
                    remainingXpLevelRes == null ||
                    (parseInt(nextLevelCostRes[1]!) - parseInt(xpLevelRes[1]!) !== parseInt(remainingXpLevelRes[1]!))
                ) {
                    if (remainingXpLevelRes != null || (remainingXpLevelRes == null && levelBtn.lore[levelBtn.lore.length - 3] !== "You have enough XP.")) {
                        throw new MumbleError("", "", window);
                    }
                }
            }
            if (salaryBtn.customName !== "Collect salary" || salaryBtn.lore.length !== 5) {
                throw new MumbleError("", "", window);
            }
            const costSalaryRes = /^Cost: (\d+) XP$/.exec(salaryBtn.lore[0]!);
            const rewardRes = /^Reward: (\d+)x (.*)$/.exec(salaryBtn.lore[1]!);
            const xpSalaryRes = /^Your XP: (\d+)$/.exec(salaryBtn.lore[2]!);
            const remainingXpSalaryRes = /^Need (\d+) more XP\.$/.exec(salaryBtn.lore[3]!);
            if (costSalaryRes == null || rewardRes == null || xpSalaryRes == null || salaryBtn.lore[4] !== "Click to collect salary" || parseInt(rewardRes[1]!) !== 64 || (parseInt(costSalaryRes[1]!) !== 500)) {
                throw new MumbleError("", "", window);
            }
            if(remainingXpSalaryRes == null || ((parseInt(costSalaryRes[1]!) - parseInt(remainingXpSalaryRes[1]!)) !== parseInt(xpSalaryRes[1]!))) {
                if(remainingXpSalaryRes != null || (remainingXpSalaryRes == null && salaryBtn.lore[3] !== "You have enough XP.")) {
                    throw new MumbleError("", "", window);
                } 
            }
            const levelMsg = levelBtn.lore.slice(2, levelBtn.lore.length - levelMax).join(" ");
            jobInfo = {
                level: parseInt(levelRegexRes[1]!),
                levelMsg,
                xp: parseInt(xpSalaryRes[1]!),
                nextLevelCost: nextLevelCostRes ? parseInt(nextLevelCostRes[1]!) : null,
                salary: rewardRes[2]!
            };
            const CheckLvl = (lvl: number, nextCost: number) => {
                if (jobInfo!.level === lvl) {
                    if (jobInfo!.nextLevelCost !== nextCost) {
                        throw new MumbleError("", "unexpected mine settings", window);
                    }
                }
            };
            CheckLvl(0, 500);
            CheckLvl(1, 1000);
            CheckLvl(2, 1500);
            CheckLvl(3, 2000);
            CheckLvl(4, 2500);
        }
        CheckSlot(window.slots[MinerJobsMenuSlots.CLOSE_MENU], "Close Menu", [
            "Return to gameplay",
        ]);

        return {page, maxPages, jobInfo};
    },
);



async function waitMove(bot: BotWithLog, position: Vec3) {
    const task = createTask();
    const listener = () => {
        bot.print(`moving from ${bot.entity.position} to ${position}`);
        if (bot.entity.position.distanceTo(position) < 10) {
            bot.removeListener("move", listener);
            bot.print(`moved from ${bot.entity.position} to ${position}`);
            task.finish();
        }
    };
    bot.on("move", listener);
    setTimeout(() => {
        bot.removeListener("move", listener);
        task.cancel(new Error(`Can't get from ${bot.entity.position} to ${position}`));
    }, 10000);
    bot.print(`waiting for ${bot.entity.position} to ${position}`);
    await bot.wrap(task.promise);
    await bot.wrap(bot.waitForChunksToLoad());
}

class GoalGoldOre extends goals.Goal {
    bot: BotWithLog;
    blockFinal: Block | null;

    constructor(bot: BotWithLog) {                                  
        super();
        this.bot = bot;
        this.blockFinal = null;                                                                                                  
    }                                                           
    heuristic() { return 0 } 
    isEnd(node: Move): boolean {
        const faces = [
            new Vec3(1,0,0),
            new Vec3(-1,0,0),
            new Vec3(0,1,0),                                                  
            new Vec3(0,-1,0),
            new Vec3(0,0,1),
            new Vec3(0,0,-1)
        ];                                                 
        if(faces.some(face => {
            const block = this.bot.blockAt((node as any).plus(face));
            return block && block.name === "gold_ore";            
        })) {
            this.blockFinal = this.bot.blockAt((node as any));
            return true;
        } else {
            return false;
        }                                        
    }                                                           
}

async function CheckBreakBlocks(nBlocks: number, botMiner: BotWithLog, xpPerBlock: number) {
    const brokenBySomebody: Set<string> = new Set();
    const func = ((block: Block, state: number, entity: Entity) => {
        if ((entity as any)?.username !== botMiner.username) {
            brokenBySomebody.add(JSON.stringify(block.position));
        }
    }) as any;
    botMiner.addListener(
        "blockBreakProgressObserved",
        func
    );
    botMiner.addListener(
        "blockBreakProgressEnd",
        func
    );
    const beforeNum = botMiner.experience.points;

    let targetBlocks: Vec3[] = [];
    let it = 0;
    while(it < nBlocks) {
        botMiner.print(`found ${targetBlocks.length} blocks`)
        if(targetBlocks.length === 0) {
            await botMiner.pathfinder.goto(new GoalGoldOre(botMiner));
            targetBlocks = botMiner.findBlocks({
                matching: block => block?.name === "gold_ore",
                maxDistance: 2,
                count: 128
            }).filter(x=>botMiner.canDigBlock(botMiner.blockAt(x)!));
        }
        const targetBlock = botMiner.blockAt(targetBlocks.shift()!)!;
        const expEvent = botMiner.wrap(once(botMiner, "experience", 3000) as unknown as Promise<void>);
        if(!botMiner.canDigBlock(targetBlock)) {
            continue;
        }
        try {
            await botMiner.dig(targetBlock, true);
            await expEvent;
        } catch(err) {
            if (!brokenBySomebody.has(JSON.stringify(targetBlock.position))) {
                throw new MumbleError("", "");
            }
            continue;
        }
        ++it;
    }
    botMiner.removeListener("blockBreakProgressObserved", func);
    botMiner.removeListener("blockBreakProgressEnd", func);
    if (botMiner.experience.points !== (beforeNum + xpPerBlock * nBlocks)) {
        throw new MumbleError(`${botMiner.experience.points} !== ${beforeNum} + ${xpPerBlock} * ${nBlocks}`, "unexpected miner experience");
    }
}

class MinerInterface {
    bot: BotWithLog;
    state: {tag: "closed"} | {tag: "main", page: number, maxPages: number, window: Window};

    constructor(bot: BotWithLog) {
        this.bot = bot;
        this.state = {tag: "closed"};
    }

    async NextPage() {
        if(this.state.tag !== "main") {
            throw new Error();
        }
        const inventoryOpen = this.bot.wrap(
            once(this.bot, "windowOpen") as unknown as Promise<
                [Window]
            >,
        );
        await this.bot.simpleClick.leftMouse(
            MinerJobsMenuSlots.NEXT_PAGE,
        );
        const [window] = await inventoryOpen;
        this.state.window = window;
        const info = CheckMinerMenu(this.state.window, this.bot.username);
        this.state.page = info.page;
        this.state.maxPages = info.maxPages;
    }

    async PrevPage() {
        if(this.state.tag !== "main") {
            throw new MumbleError("", "");
        }
        const inventoryOpen = this.bot.wrap(
            once(this.bot, "windowOpen") as unknown as Promise<
                [Window]
            >,
        );
        await this.bot.simpleClick.leftMouse(
            MinerJobsMenuSlots.PREVIOUS_PAGE,
        );
        const [window] = await inventoryOpen;
        this.state.window = window;
        const info = CheckMinerMenu(this.state.window, this.bot.username);
        this.state.page = info.page;
        this.state.maxPages = info.maxPages;
    }
    
    async MainMenu() {
        switch(this.state.tag) {
            case "closed": {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.chat("/work");
                const [window] = await inventoryOpen;
                this.state = {tag: "main", page: -1, maxPages: -1, window: window};
                break;
            }
            case "main": {
                break;
            }
        }
        const menuRes = CheckMinerMenu(this.state.window, this.bot.username)!;
        this.state = {tag: "main", page: menuRes.page, maxPages: menuRes.maxPages, window: this.state.window};
        JobList(this.state.window);
        return menuRes;
    }

    async FirstPage() {
        switch(this.state.tag) {
            case "closed": {
                await this.MainMenu();
                break;
            }
            case "main": {
                while(this.state.page !== 1) {
                    await this.PrevPage();
                }
                break;
            }
        }
    }

    async SelectJob(ownerNameToFind: string): Promise<{salary: string, xp: number}> {
        await this.FirstPage();
        if(this.state.tag !== "main") {
            throw new MumbleError("", "unexpected miner interface");
        }

        let ownerSlot: {active: boolean, index: number, owner: string, salary: string, xp: number} | null = null;
        ownerSlot = JobList(this.state.window)!.find(x => x.owner === ownerNameToFind) ?? null;
        while(this.state.page != this.state.maxPages) {
            ownerSlot = JobList(this.state.window)!.find(x => x.owner === ownerNameToFind) ?? null;
            if (ownerSlot != null) {
                break;
            }
            await this.NextPage();
        }
        if(ownerSlot == null) {
            writeToLog(Bun.inspect(this.state.window.slots));
            throw new MumbleError("", "", this.state.window);
        }
        const inventoryClose = this.bot.wrap(once(this.bot, "windowClose") as unknown as Promise<[Window]>);
        await this.bot.simpleClick.leftMouse(ownerSlot.index);
        await inventoryClose;
        this.state = {tag: "closed"};
        return {salary: ownerSlot.salary, xp: ownerSlot.xp};
    }

    async TeleportToJobPlace() {
        await this.MainMenu();
        if(this.state.tag !== "main") {
            throw new MumbleError("", "unexpected miner interface");
        }
        JobList(this.state.window).find(x => x.active);
        let ownerSlot: {active: boolean, index: number, owner: string, salary: string, xp: number} | null = null;
        ownerSlot = JobList(this.state.window).find(x => x.active) ?? null;
        while(this.state.page != this.state.maxPages) {
            ownerSlot = JobList(this.state.window).find(x => x.active) ?? null;
            if (ownerSlot != null) {
                break;
            }
            await this.NextPage();
        }
        if(ownerSlot == null) {
            throw new MumbleError("", "unexpected miner interface");
        }
        // "Teleported to " + newEmployer.ownerName() + "'s mine."
        const inventoryClose = this.bot.wrap(once(this.bot, "windowClose") as unknown as Promise<[Window]>);
        const tpMsg = this.bot.awaitMessage(/^Teleported to (.*)'s mine\.$/);
        await this.bot.simpleClick.leftMouse(ownerSlot.index);
        await inventoryClose;
        this.state = {tag: "closed"};

        const msg = await this.bot.wrap(withTimeout(tpMsg, 5000));
        const msgOwner = /^Teleported to (.*)'s mine\.$/.exec(msg);
        if (msgOwner == null || msgOwner[1] !== ownerSlot.owner) {
            throw new MumbleError("", "");
        }
    }
    async UpgradeLevel(expectedText: string | undefined = undefined) {
        const prevData = await this.MainMenu();
        if(this.state.tag !== "main") {
            throw new Error();
        }
        const inventoryOpen = this.bot.wrap(
            once(this.bot, "windowOpen") as unknown as Promise<
                [Window]
            >,
        );
        await this.bot.simpleClick.leftMouse(
            MinerJobsMenuSlots.LEVEL,
        );
        const [window] = await inventoryOpen;
        this.state.window = window;
        const newData = await this.MainMenu();
        if (prevData.jobInfo!.level + 1 !== newData.jobInfo!.level) {
            throw new MumbleError("", "miner upgrade doesn't work", this.state.window);
        }
        if(expectedText && expectedText !== newData.jobInfo?.levelMsg) {
            throw new MumbleError(expectedText, "", this.state.window);
        }
        return newData;
    }

    async BuySalary() {
        await this.MainMenu();
        if(this.state.tag !== "main") {
            throw new Error();
        }
        const inventoryOpen = this.bot.wrap(
            once(this.bot, "windowOpen") as unknown as Promise<
                [Window]
            >,
        );
        await this.bot.simpleClick.leftMouse(
            MinerJobsMenuSlots.SALARY,
        );
        const [window] = await inventoryOpen;
        this.state.window = window;
        await this.MainMenu();
    }

    async Close() {
        switch(this.state.tag) {
            case "closed": {
                break;
            }
            case "main": {
                const inventoryClose = this.bot.wrap(
                    once(this.bot, "windowClose") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    MinerJobsMenuSlots.CLOSE_MENU,
                );
                await inventoryClose;
                this.state = {tag: "closed"};
                break;
            }
        }
    }
        // const levelBtn = GetSlotInfo();
        // const salaryBtn = GetSlotInfo(window.slots[MinerJobsMenuSlots.SALARY]);
}

class OwnerInterface {
    bot: BotWithLog;
    state: {tag: "closed"} | {tag: "main", window: Window} | {tag: "salary", window: Window} | {tag: "manage", window: Window};

    constructor(bot: BotWithLog) {
        this.bot = bot;
        this.state = {tag: "closed"};
    }

    async MainMenu() {
        switch(this.state.tag) {
            case "closed": {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.chat("/work");
                const [window] = await inventoryOpen;
                this.state = {tag: "main", window: window};
                break;
            }
            case "main": {
                break;
            }
            case "salary": {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    OwnerSalarySelectionSlots.BACK,
                );
                let [window] = await inventoryOpen;
                this.state = {tag: "main", window};
                break;
            }
            case "manage": {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    OwnerMinersMenuSlots.BACK,
                );
                let [window] = await inventoryOpen;
                this.state = {tag: "main", window};
                break;
            }
        }
        return CheckOwnerMainMenu(this.state.window, this.bot.username);
    }

    async SalarySelection() {
        switch(this.state.tag) {
            case "closed": {
                await this.MainMenu();
                await this.SalarySelection();
                break;
            }
            case "main": {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    OwnerMainMenuSlots.SALARY_ITEM,
                );
                const [window] = await inventoryOpen;
                CheckOwnerSalarySelection(window);
                this.state = {tag: "salary", window};
                break;
            }
            case "salary": {
                break;
            }
            case "manage": {
                await this.MainMenu();
                await this.SalarySelection();
            }
        }
    }

    async SelectSalary(slotIndex: number) {
        await this.SalarySelection();
        const inventoryOpen = this.bot.wrap(
            once(this.bot, "windowOpen") as unknown as Promise<
                [Window]
            >,
        );
        if (this.state.tag !== "salary") {
            throw new Error();
        }
        const itemName = this.state.window.slots[slotIndex]?.name;
        await this.bot.simpleClick.leftMouse(
            slotIndex,
        );
        const [window] = await inventoryOpen;
        this.state = {tag: "main", window: window};
        const menuRes = await this.MainMenu();
        this.bot.print(`Selected ${itemName} salary`);
        if (itemName !== menuRes.salary.toLowerCase()) {
            throw new MumbleError(`${Bun.inspect(itemName)} != ${Bun.inspect(menuRes)}`, "", window);
        }
        return menuRes;
    }

    async SetText(num: 0|1|2|3|4|5, text: string) {
        await this.MainMenu();
        if (this.state.tag === "closed") {
            throw new Error();
        }
        const inventoryClose = this.bot.wrap(
            once(this.bot, "windowClose") as unknown as Promise<
                [Window]
            >,
        );
        await this.bot.simpleClick.leftMouse(
            {
                0: OwnerMainMenuSlots.LEVEL_0_MESSAGE,
                1: OwnerMainMenuSlots.LEVEL_1_MESSAGE,
                2: OwnerMainMenuSlots.LEVEL_2_MESSAGE,
                3: OwnerMainMenuSlots.LEVEL_3_MESSAGE,
                4: OwnerMainMenuSlots.LEVEL_4_MESSAGE,
                5: OwnerMainMenuSlots.LEVEL_5_MESSAGE,
            }[num]
        );
        await inventoryClose;
        this.state = {tag: "closed"};
        const inventoryOpen = this.bot.wrap(
            once(this.bot, "windowOpen") as unknown as Promise<
                [Window]
            >,
        );
        await this.bot.chat(text);
        const [window] = await inventoryOpen;
        this.state = {tag: "main", window};
    }

    async ManageWorkers() {
        switch(this.state.tag) {
            case "closed": {
                await this.MainMenu();
                await this.ManageWorkers();
                break;
            }
            case "main": {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    OwnerMainMenuSlots.MANAGE_WORKERS,
                );
                const [window] = await inventoryOpen;
                this.state = {tag: "manage", window};
                CheckOwnerMiners(window);
                break;
            }
            case "salary": {
                await this.MainMenu();
                await this.ManageWorkers();
                break;
            }
            case "manage": {
                break;
            }
        }
    }

    async HelpWorker(worker: string): Promise<boolean> {
        await this.ManageWorkers();
        if (this.state.tag !== "manage") {
            throw new Error();
        }
        for(const slot of this.state.window.slots.slice(OwnerMinersMenuSlots.WORKER_START, OwnerMinersMenuSlots.WORKER_END).filter(x=>x)) {
            const data = GetSlotInfo(slot);
            const minerNameRes = /^Worker: (.*)$/.exec(data.customName);
            if(minerNameRes == null) {
                throw new MumbleError("", "unexpected owner interface", this.state.window);
            }
            if(minerNameRes[1] === worker) {
                const inventoryOpen = this.bot.wrap(
                    once(this.bot, "windowOpen") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    slot!.slot,
                );
                const [window] = await inventoryOpen;
                this.state.window = window;
                writeToLog(`found help ${Bun.inspect(minerNameRes)}`);
                return true;
            }
        }
        
        return false;
    }

    async SetXp(xp: number) {
        let ownerState = await this.MainMenu();
        if (this.state.tag === "closed") {
            throw new Error();
        }
        while(xp !== ownerState.xp) {
            const inventoryOpen = this.bot.wrap(
                once(this.bot, "windowOpen") as unknown as Promise<
                    [Window]
                >,
            );
            await this.bot.clickWindow(
                OwnerMainMenuSlots.XP_PER_BLOCK,
                ownerState.xp < xp ? 0 : 1,
                Math.abs(ownerState.xp - xp) < 10 ? 0 : 1
            );
            const [window] = await inventoryOpen;
            this.state.window = window;
            ownerState = await this.MainMenu();
        }
    }

    async Close() {
        switch(this.state.tag) {
            case "closed": {
                break;
            }
            case "main": {
                const inventoryClose = this.bot.wrap(
                    once(this.bot, "windowClose") as unknown as Promise<
                        [Window]
                    >,
                );
                await this.bot.simpleClick.leftMouse(
                    OwnerMainMenuSlots.CLOSE_MENU,
                );
                await inventoryClose;
                this.state = {tag: "closed"};
                break;
            }
            case "salary": {
                await this.MainMenu();
                await this.Close();
                break;
            }
            case "manage": {
                await this.MainMenu();
                await this.Close();
                break;   
            }
        }
    }
}

interface FlagId {
    session: mcproto.SessionOption,
    home: Vec3,
}

async function main() {
    const args = argv.slice(2);
    if (args.length < 2) {
        writeToLog(`Not enough arguments ${args}`);
        exitProcess("CHECKER_ERROR");
    }
    const address = args[1] as string;
    try {
        switch (args[0]) {
            case "check": {
                if (args.length === 2) {
                    const userdataOwner: RegisteredUserData =
                        await registerUser(address, {
                            username: randomString(8),
                            role: "businessman",
                        });
                    let sessionOwner = await yggdrasil.loginWithOtp(
                        address,
                        userdataOwner,
                    );
                    const tokens = {
                        accessToken: sessionOwner.accessToken,
                        clientToken: sessionOwner.clientToken,
                    };
                    writeToLog(
                        `tokens for sessionOwner: ${JSON.stringify(tokens)}`,
                    );

                    await yggdrasil.validate(address, {
                        ...tokens,
                        expectSuccess: true,
                    });
                    if (randomInt(2) == 0) {
                        await yggdrasil.invalidate(address, {
                            ...tokens,
                            expectSuccess: true,
                        });
                        await yggdrasil.validate(address, {
                            ...tokens,
                            expectSuccess: false,
                        });
                        sessionOwner = await yggdrasil.loginWithOtp(
                            address,
                            userdataOwner,
                        );
                    }

                    const botOwner = await joinServer({
                        session: sessionOwner,
                        address,
                        version: "1.21.8",
                    });
                    botOwner.home = botOwner.entity.position.clone();

                    const ownerInterface = new OwnerInterface(botOwner);
                    const ownerInfo = await ownerInterface.MainMenu();
                    const defaultLevel0 = "Welcome to the mine!";
                    const defaultLevel1 = "Keep mining, you are getting better.";
                    const defaultLevel2 = "Solid progress, miner.";
                    const defaultLevel3 = "Veteran of the shafts.";
                    const defaultLevel4 = "Almost a legend underground.";
                    const defaultLevel5 = "Master of the mine!";

                    if (ownerInfo.xp !== 10 || ownerInfo.salary !== "GOLD_INGOT" ||
                        ownerInfo.level0 !== defaultLevel0 ||
                        ownerInfo.level1 !== defaultLevel1 ||
                        ownerInfo.level2 !== defaultLevel2 ||
                        ownerInfo.level3 !== defaultLevel3 ||
                        ownerInfo.level4 !== defaultLevel4 ||
                        ownerInfo.level5 !== defaultLevel5
                    ) {
                        throw new MumbleError(Bun.inspect(ownerInfo), "unexpected mine settings");
                    }

                    await ownerInterface.SalarySelection();
                    await ownerInterface.ManageWorkers();
                    await ownerInterface.Close();

                    const nMax = randomInt(1, 2);
                    for(let i=0; i < nMax; ++i) {
                        writeToLog(`spawning worker ${i + 1}`);
                        await sleep(2000 * i + randomInt(5) * 1000);
                        const botMiner = await getRandomBot(
                            address,
                            "miner",
                        );
                        const pickaxeSlot = botMiner.inventory.slots.find((item: Item | null) => item?.name === "diamond_pickaxe");
                        if (pickaxeSlot == null) {
                            throw new MumbleError("", "can't find pickaxe");
                        }
                        await botMiner.equip(pickaxeSlot, "hand");
                        const minerInterface = new MinerInterface(botMiner);
                        const jobInfo = await minerInterface.SelectJob(botOwner.username);
                        await sleep(1000);
                        let menuInfo = await minerInterface.MainMenu();
                        if (jobInfo.salary !== "GOLD_INGOT" || jobInfo.xp !== 10 || menuInfo.jobInfo?.levelMsg !== defaultLevel0) {
                            throw new MumbleError(`${Bun.inspect(jobInfo)} ${Bun.inspect(menuInfo)}`, "unexpected mine settings");
                        }

                        const nItem = randomInt(9);
                        await ownerInterface.SelectSalary(nItem);
                        await ownerInterface.Close();
                        
                        await botMiner.wrap(waitMove(botMiner, botOwner.home));
                        await botMiner.wrap(botMiner.waitForChunksToLoad());

                        await CheckBreakBlocks(1, botMiner, jobInfo.xp);

                        const expEvent = botMiner.wrap(once(botMiner, "experience") as unknown as Promise<void>);
                        if (await ownerInterface.HelpWorker(botMiner.username)) {
                            await expEvent;
                            if(botMiner.experience.points !== jobInfo.xp + 1000) {
                                throw new MumbleError(`${Bun.inspect(botMiner.experience)} ${Bun.inspect(menuInfo)}`, "");
                            }
                        }

                        const xpPerBlockNew = randomInt(400, 600);
                        await ownerInterface.SetXp(xpPerBlockNew);

                        await CheckBreakBlocks(Math.ceil((8000 - botMiner.experience.points) / xpPerBlockNew), botMiner, xpPerBlockNew);
                        menuInfo = await minerInterface.UpgradeLevel(defaultLevel1);
                        menuInfo = await minerInterface.UpgradeLevel(defaultLevel2);
                        menuInfo = await minerInterface.UpgradeLevel(defaultLevel3);
                        menuInfo = await minerInterface.UpgradeLevel(defaultLevel4);
                        const flag = randomString(32);
                        await ownerInterface.SetText(5, flag);
                        await ownerInterface.Close();
                        menuInfo = await minerInterface.UpgradeLevel(flag);
                        
                        await minerInterface.Close();

                        await botMiner.chat("/home");
                        await botMiner.wrap(waitMove(botMiner, botMiner.home));

                        const updateSlot = botMiner.wrap(
                            once(botMiner.inventory, "updateSlot" as any) as unknown as Promise<
                                [number, Item | null, Item]
                            >,
                        );
                        await minerInterface.BuySalary();
                        await minerInterface.Close();
                        await updateSlot;
                        const salaries = botMiner.inventory.slots.filter(x=>x?.name?.toUpperCase() === menuInfo.jobInfo?.salary && x?.count === 64);
                        if (salaries.length < 1) {
                            throw new MumbleError(`${Bun.inspect(botMiner.inventory)} != ${Bun.inspect(menuInfo)}`, "unexpected salary");
                        }
                        writeToLog(`final block ${Bun.inspect(menuInfo)}`)
                        if (["COBBLESTONE", "OAK_LOG"].find(x => x === menuInfo.jobInfo?.salary)) {
                            await botMiner.equip(salaries[0]!, "hand");
                            const blockTarget = await botMiner.findBlock({point: botMiner.entity.position.clone().add(new Vec3(1, 1, 0)), matching: (x) => x.name.toUpperCase() === "SMOOTH_STONE", maxDistance: 2});
                            if (blockTarget == null) {
                                throw new MumbleError(`${blockTarget} can't find SMOOTH`, "unexpected worker house");
                            }
                            await botMiner.placeBlock(blockTarget!, new Vec3(0, 1, 0));
                        } else {
                            // craft
                        }

                        await minerInterface.TeleportToJobPlace();
                        await botMiner.wrap(waitMove(botMiner, botOwner.home));

                        botMiner.print("Done!");
                        botMiner.quit();
                    }
                    botOwner.quit();
                    const newSessionOwner = await yggdrasil.loginWithOtp(
                        address,
                        userdataOwner,
                    );

                    exitProcess("OK");
                } else {
                    writeToLog(`Invalid count of args: ${args}`);
                    exitProcess("CHECKER_ERROR");
                }
                break;
            }
            case "put": {
                if (args.length == 5) {
                    // flag_id, flag, vuln
                    const flag = args[3]!;
                    if (args[4] === "1") {
                        const botOwner = await getRandomBot(address, "businessman");
                        const ownerInterface = new OwnerInterface(botOwner);
                        await ownerInterface.SetText(5, flag);
                        await ownerInterface.Close();
                        // private flag_id
                        if (botOwner._client.session == null) {
                            botOwner.print("Session is null");
                            exitProcess("CHECKER_ERROR");
                        } else {
                            logPlayersAround(botOwner);
                            const flagId: FlagId = {session: botOwner._client.session, home: botOwner.home};
                            writeToLog(`Saving flagId ${JSON.stringify(flagId)}`);
                            writeFileSync(privateOutputFile, btoa(JSON.stringify(flagId)));
                            writeFileSync(publicOutputFile, botOwner.username);
                            botOwner.quit();
                            exitProcess("OK");
                        }
                    } else {
                        writeToLog(`Got unexpected vuln ${args[4]}`);
                        exitProcess("CHECKER_ERROR");
                    }
                } else {
                    writeToLog(`Invalid count of args: ${args}`);
                    exitProcess("CHECKER_ERROR");
                }
                break;
            }
            case "get": {
                if (args.length == 5) {
                    // flag_id, flag, vuln
                    const flag = args[3]!;
                    const flagId = JSON.parse(atob(args[2]!)) as FlagId;
                    flagId.home = new Vec3(flagId.home.x, flagId.home.y, flagId.home.z);
                    if (args[4] === "1") {
                        const botOwner = await joinServer({session: flagId.session, address, version: "1.21.8"});
                        const ownerInterface = new OwnerInterface(botOwner);
                        const menuData = await ownerInterface.MainMenu();
                        if (flag !== menuData.level5) {
                            writeToLog(`${Bun.inspect(menuData)} != ${flag}`);
                            exitProcess("CORRUPT");
                        } else {
                            exitProcess("OK");
                        }
                    } else {
                        writeToLog(`Got unexpected vuln ${args[4]}`);
                        exitProcess("CHECKER_ERROR");
                    }
                } else {
                    writeToLog(`Invalid count of args: ${args}`);
                    exitProcess("CHECKER_ERROR");
                }
                break;
            }
            default: {
                writeToLog(`Unknown mode ${args[0]}`);
                exitProcess("CHECKER_ERROR");
            }
        }
    } catch (err) {
        if (err instanceof MumbleError) {
            writeToLog(`Got MUMBLE type error: ${Bun.inspect(err)}`);
            if (err.checkerText != null && err.checkerText.length !== 0) {
                writeFileSync(publicOutputFile, err.checkerText);
            }
            exitProcess("MUMBLE");
        } else if (err instanceof DownError) {
            writeToLog(`Got DOWN type error: ${Bun.inspect(err)}`);
            exitProcess("DOWN");
        } else {
            writeToLog(`Got unknown error: ${Bun.inspect(err)}`);
            exitProcess("CHECKER_ERROR");
        }
    }
}

const publicOutputFile = env.publicOutput!;
if (publicOutputFile == null) {
    writeToLog("env.publicOutput is missing");
    exitProcess("CHECKER_ERROR");
}

const privateOutputFile = env.privateOutput!;
if (privateOutputFile == null) {
    writeToLog("env.privateOutput is missing");
    exitProcess("CHECKER_ERROR");
}

const logOutputFile = env.logOutput!;
if (logOutputFile == null) {
    writeToLog("env.logOutput is missing");
    exitProcess("CHECKER_ERROR");
}

function writeToLog(text: string) {
    const time = new Date().toLocaleTimeString("de-DE");
    appendFileSync(logOutputFile!, `${time} ${text}\n`);
}

await main();
