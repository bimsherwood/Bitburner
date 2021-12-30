/** @param {NS} ns **/

import { safeLoop } from "utils.js";

export async function main(ns) {
  await safeLoop(ns, async function(){ ; })
}
