/** @param {NS} ns **/

import { safeLoop } from "./utils.js";

function Thief (ns, target) {
  
  var lastMoney = ns["getServerMoneyAvailable"](target);
  var maxMoney = ns["getServerMaxMoney"](target) * 0.95;
  
  var lastSecurity = ns["getServerSecurityLevel"](target);
  var minSecurity = ns["getServerMinSecurityLevel"](target) + 1;

  async function heuristic(){
    
    var currentMoney = ns["getServerMoneyAvailable"](target);
    var targetMoney = Math.min(maxMoney, lastMoney * 1.05 + 1);
    
    var currentSecurity = ns["getServerSecurityLevel"](target);
    var targetSecurity = Math.max(minSecurity, lastSecurity * 0.8);
    
    ns.print("### Target security: ", currentSecurity, " / ", targetSecurity);
    ns.print("### Target money: ", currentMoney, " / ", targetMoney);
    
    if (currentSecurity > targetSecurity) {
      await ns["weaken"](target);
    } else if (currentMoney < targetMoney) {
      await ns["grow"](target);
    } else {
      lastSecurity = currentSecurity;
      lastMoney = currentMoney;
      await ns["hack"](target);
    }
    
    await ns.sleep(Math.floor(1000*Math.random()));
    
  }

  async function start() {
    await safeLoop(ns, heuristic);
  }

  return {
    start
  };

};

function printHelp(ns){
  ns["tprint"]("Usage:");
  ns["tprint"]("  thief.js <target>");
}

export async function main(ns) {
  
  var argc = ns.args.length;
  if(argc != 1){
    printHelp(ns);
    return;
  }
  
  var target = ns.args[0];
  var thief = new Thief(ns, target);
  await thief.start();
  
}