/** @param {NS} ns **/

import { safeLoop, portReceive } from "./utils.js";

export async function main(ns){
  await safeLoop(ns, async function(){
    var result = await portReceive(ns, "test");
    ns.tprint(result);
  });
}