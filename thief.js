/** @param {NS} ns **/

import { safeLoop } from "./utils.js";

function Thief (ns, target) {
  
  var lastMoney = ns.getServerMoneyAvailable(target);
  var lastSecurity = ns.getServerSecurityLevel(target);
  var securityMin = ns.getServerMinSecurityLevel(target) + 5;
  var moneyMax = ns.getServerMaxMoney(target) * 0.75;

  async function heuristic(){
    var security = ns.getServerSecurityLevel(target);
    var money = ns.getServerMoneyAvailable(target);
    if (security >= lastSecurity && security > securityMin) {
      await ns.weaken(target);
    } else if (money <= lastMoney && money < moneyMax) {
      await ns.grow(target);
    } else {
      lastSecurity = security;
      lastMoney = money;
      await ns.hack(target);
    }
  }

  async function run() {
    await safeLoop(ns, heuristic);
  }

  return {
    run
  };

};

function printHelp(ns){
  ns.print("Usage:");
  ns.print("  thief.js <target>");
}

export async function main(ns) {
  
  var argc = ns.args.length;
  if(argc != 1){
    printHelp(ns);
    return;
  }
  
  var target = ns.args[0];
  var thief = new Thief(ns, target);
  await thief.run();
  
}
