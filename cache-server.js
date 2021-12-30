/** @param {NS} ns **/

/**
 * Stores data in memory between instances of the same script. Does NOT support
 * inter-script communication, only inter-instance communication.
 */

import { safeLoop } from "./utils.js";

var caches = { }

export function openCache(name){
  
  // Ensure database exists
  if (typeof(caches[name]) == "undefined"){
    caches[name] = { };
  }
  
  return caches[name];
  
}

export async function main(ns) {
  await safeLoop(ns, async function(){
    await ns.sleep(60*1000);
  });
}