/** @param {NS} ns **/

import { DatabaseClient } from "./database-client.js";
import { forEachAsync } from "./utils.js";

export function InstallCell (ns){
  
  var maxInstances = 8;
  var databaseName = "cell";
  var mainScript = "cell.js";
  var scripts = [
    "utils.js",
    "database-client.js",
    "cell.js"
  ];
  
  async function install(hostname){
    
    await forEachAsync(scripts, async function(i,e){
      await ns.scriptKill(e, hostname);
    });
    await forEachAsync(scripts, async function(i,e){
      await ns.scp(e, hostname);
    });
    
    var instances = [];
    var ramRequired = await ns.getScriptRam(mainScript);
    var ramMax = await ns.getServerMaxRam(hostname);
    var ramUsed = await ns.getServerUsedRam(hostname);
    var ramAvailable = ramMax - ramUsed;
    var totalThreadCount = Math.floor(ramAvailable / ramRequired);
    var instanceCount = Math.min(maxInstances, totalThreadCount);
    for(var i = 0; i < instanceCount; i++){
      var threadCount = Math.floor(totalThreadCount / instanceCount);
      var instanceName = "cell-" + i;
      await ns.sleep(100);
      await ns.exec(
        mainScript,
        hostname,
        threadCount,
        databaseName,
        instanceName);
      instances.push({
        hostname,
        instance: instanceName,
        threads: threadCount
      });
    }
    
    var database = new DatabaseClient(ns, databaseName);
    await database.write("cells", instances);
    
  }
  
  async function uninstall(hostname){
    await forEachAsync(scripts, async function(i,e){
      await ns.scriptKill(e, hostname);
    });
    await forEachAsync(scripts, async function(i,e){
      await ns.rm(e, hostname);
    });
  }
  
  return {
    install,
    uninstall
  };
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  install-cell.js install <host>");
  ns.tprint("  install-cell.js uninstall <host>");
}

export async function main(ns) {
  var installCell = new InstallCell(ns);
  if(ns.args.length == 2 && ns.args[0] == "install"){
    await installCell.install(ns.args[1]);
  } else if(ns.args.length == 2 && ns.args[0] == "uninstall"){
    await installCell.uninstall(ns.args[1]);
  } else {
    printHelp(ns);
  }
}