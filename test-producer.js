/** @param {NS} ns **/

import { portSend } from "./utils.js";

export async function main(ns){
  await portSend(ns, "test", ns.args);
}