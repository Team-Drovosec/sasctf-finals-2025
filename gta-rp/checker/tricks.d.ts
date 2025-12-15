import type { BotEvents } from "mineflayer";


export async function once<E extends keyof BotEvents>(bot: Bot, event: E, timeout: number = 20000): BotEvents[E];
export async function withTimeout<T>(promise: Promise<T>, timeout: number): Promise<T>;
export function createTask(): any;
