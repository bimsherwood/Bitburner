/** @param {NS} ns **/

import { forEachAsync } from "utils.js";

export function InstallAgent (ns){
  
  var mainScript = "agent.js"
  var scripts = [
    "utils.js",
    "database-client.js",
    "agent.js"
  ];
  
  async function stop(hostname){
    await forEachAsync(scripts, async function(i,e){
      await ns.scriptKill(e, hostname);
    });
  }
  
  async function install(hostname){
    await stop(hostname);
    await forEachAsync(scripts, async function(i,e){
      await ns.scp(e, hostname);
    });
  }
  
  async function run(hostname, databaseName, jobName){
    var ramRequired = await ns.getScriptRam(mainScript);
    var ramMax = await ns.getServerMaxRam(hostname);
    var ramUsed = await ns.getServerUsedRam(hostname);
    var ramAvailable = ramMax - ramUsed;
    var threadCount = Math.floor(ramAvailable / ramRequired);
    await ns.exec(
      mainScript,
      hostname,
      threadCount,
      databaseName,
      jobName);
  }
  
  async function uninstall(hostname){
    await stop(hostname);
    await forEachAsync(scripts, async function(i,e){
      await ns.rm(e, hostname);
    });
  }
  
  return {
    install,
    stop,
    run,
    uninstall
  };
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  install-cell.js install <host>");
  ns.tprint("  install-cell.js run <host> <database> <key of a function>");
  ns.tprint("  install-cell.js stop <host>");
  ns.tprint("  install-cell.js uninstall <host>");
}

export async function main(ns) {
  var installer = new InstallAgent(ns);
  if(ns.args.length == 2 && ns.args[0] == "install"){
    await installer.install(ns.args[1]);
  } else if(ns.args.length == 4 && ns.args[0] == "run"){
    await installer.run(ns.args[1], ns.args[2], ns.args[3]);
  } else if(ns.args.length == 2 && ns.args[0] == "stop"){
    await installer.stop(ns.args[1]);
  } else if(ns.args.length == 2 && ns.args[0] == "uninstall"){
    await installer.uninstall(ns.args[1]);
  } else {
    printHelp(ns);
  }
}