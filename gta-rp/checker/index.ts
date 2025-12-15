import {appendFileSync, existsSync, mkdirSync, mkdtempSync, readFileSync, rmSync} from "node:fs";
import {argv, exit} from "node:process";
import {join} from "node:path";
import { sleep, spawn, spawnSync } from "bun";
import os from "node:os";

const tmpDir = mkdtempSync("mc_out");

const args = argv.slice();
const idx = args.findIndex((str) => str.endsWith("index.ts"));
if (idx === -1) {
    console.error(`Can't find index.ts in args ${args}`);
    rmSync(tmpDir, {recursive: true});
    exit(110);
}
args[idx] = args[idx]!.replace("index.ts", "main.ts");

const _mode = 1 <= argv.slice(2).length ? argv.slice(2)[0] : "nomode";
const _addr = 1 <= argv.slice(3).length ? argv.slice(3)[0] : "noaddr";
const _subpath = (() => {
    let prefix;
    if (os.platform() === "win32") {
        prefix = ".";
    } else {
        prefix = "/var/log";
    }
    const folder = prefix + "/mc-checker";
    if (!existsSync(folder)) {
        mkdirSync(folder, {recursive: true});
    }
    return folder;
})();

const startTime = new Date();
let nTry = 1;
const KILL_FULL_TIMEOUT = 150 - 20;
const KILL_SINGLE_TIMEOUT = 75;

let lastResult = null;

function returnResult() {
    process.stdout.write(lastResult!.publicData);
    process.stderr.write(lastResult!.privateData);
    rmSync(tmpDir, {recursive: true});
    exit(lastResult!.status);
}

try {
    while(true) {
        const getSecondsLeft = () => KILL_FULL_TIMEOUT - (new Date().getTime() - startTime.getTime()) / 1000;
        const fileLog = `${_subpath}/${new Date().toLocaleTimeString('de-DE').replaceAll(":", ".")}-${_addr}-${_mode}-${nTry}.log`;
        const filePublic = join(tmpDir, `public-${nTry}`);
        const filePrivate = join(tmpDir, `private-${nTry}`);
        
        const proc = spawn(args, {
            env: {...process.env, publicOutput: filePublic, privateOutput: filePrivate, logOutput: fileLog},
            stdout: "pipe",
            stderr: "pipe",
        });
        await Promise.race([
            proc.exited,
            sleep(Math.min(KILL_SINGLE_TIMEOUT, getSecondsLeft()) * 1000),
        ]);

        if (proc.exitCode == null) {
            appendFileSync(fileLog, "got timeout, killing");
            proc.kill();
        } else {
            lastResult = {
                publicData: existsSync(filePublic) ? readFileSync(filePublic).toString() : "",
                privateData: existsSync(filePrivate) ? readFileSync(filePrivate).toString() : "",
                status: proc.exitCode
            };
            appendFileSync(fileLog, `stdout: ${await new Response(proc.stdout).text()}\nstderr: ${await new Response(proc.stderr).text()}`);
        }
        if (lastResult?.status === 101 || getSecondsLeft() < KILL_SINGLE_TIMEOUT / 2) {
            if (lastResult) {
                // console.log(`returning ${lastResult}`);
                returnResult();
            } else {
                // console.log("Don't have result before timeout...");
                rmSync(tmpDir, {recursive: true});
                exit(110);
            }
        } else {
            // break;
            nTry += 1;
        }
    }
} finally {
    rmSync(tmpDir, {recursive: true});
}
