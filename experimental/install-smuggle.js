/** @param {NS} ns **/

import { DatabaseClient } from "database-client.js";
import { InstallAgent } from "install-agent.js";
import { safeLoop } from "utils.js";

export function InstallSmuggle (ns, agentInstaller){
  
  var databaseName = "smuggle";
  
  var databaseServerScript = "database.js";
  var databaseServerArg = "server";
  var databaseServerHost = "home";
  var database = new DatabaseClient(ns, databaseName);
  
  async function algorithmForTarget(target){
    
    var lastMoney = await ns.getServerMoneyAvailable(target);
    var maxMoney = await ns.getServerMaxMoney(target) * 0.95;
    
    var lastSecurity = await ns.getServerSecurityLevel(target);
    var minSecurity = await ns.getServerMinSecurityLevel(target) + 1;
    
    return async function(){
      await safeLoop(ns, async function(){
        var currentMoney = await ns.getServerMoneyAvailable(target);
        var targetMoney = Math.min(maxMoney, lastMoney * 1.05 + 1);
        
        var currentSecurity = await ns.getServerSecurityLevel(target);
        var targetSecurity = Math.max(minSecurity, lastSecurity * 0.95);
        
        ns.tprint(
          "### Target security: ",
          currentSecurity, " / ", targetSecurity);
        ns.tprint(
          "### Target money: ",
          currentMoney, " / ", targetMoney);
        
        if (currentSecurity > targetSecurity) {
          await ns.weaken(target);
        } else if (currentMoney < targetMoney) {
          await ns.grow(target);
        } else {
          lastSecurity = currentSecurity;
          lastMoney = currentMoney;
          await ns.hack(target);
        }
      });
    }
    
  }
  
  async function ensureDatabaseServerRunning(){
    var running = await ns.isRunning(
        databaseServerScript,
        databaseServerHost,
        databaseServerArg);
    if(!running){
      await ns.exec(
          databaseServerScript,
          databaseServerHost,
          1,
          databaseServerArg);
    }
  }
  
  async function install(host, target){
    await ensureDatabaseServerRunning();
    var algorithm = await algorithmForTarget(target);
    await agentInstaller.install(host);
    await database.write(host, { run: algorithm });
    await agentInstaller.run(host, databaseName, host);
  }
  
  return {
    install
  };
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  install-smuggle.js <host> <target>");
}


export async function main(ns) {
  var agentInstaller = new InstallAgent(ns);
  var installer = new InstallSmuggle(ns, agentInstaller);
  if (ns.args.length == 2){
    await installer.install(ns.args[0], ns.args[1]);
  } else {
    printHelp(ns);
  }
}