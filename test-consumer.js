/** @param {NS} ns **/

import { portSubscribe } from "./port.js";
import { safeLoop } from "./utils.js";

export async function main(ns){
  var subscription = await portSubscribe(ns, "test-channel");
  await safeLoop(ns, async function(){
    var result = subscription.tryRead();
    if (result.success){
      ns.tprint(result.message);
    }
  });
}