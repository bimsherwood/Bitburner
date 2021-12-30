/** @param {NS} ns **/

import { portSend } from "./port.js";

export async function main(ns){
  await portSend(ns, "test-channel", ns.args);
}