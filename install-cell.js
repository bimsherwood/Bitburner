/** @param {NS} ns **/

import { forEachAsync } from "utils.js";

// A model for an instance of Cell.js running on a server
export function Cell(hostname, index){
  
  return {
    hostname,
    index,
    instanceId: hostname + "|" + index
  };
  
}

export function InstallCell (ns){
  
  var mainScript = "cell.js";
  var scripts = [
    "utils.js",
    "cell.js"
  ];
  
  async function install(hostname){
    
    await forEachAsync(scripts, async function(i,e){
      await ns.scriptKill(e, hostname);
    });
    await forEachAsync(scripts, async function(i,e){
      await ns.scp(e, hostname);
    });
    
    var ramRequired = await ns.getScriptRam(mainScript);
    var ramMax = await ns.getServerMaxRam(hostname);
    var ramUsed = await ns.getServerUsedRam(hostname);
    var ramAvailable = ramMax - ramUsed;
    var instanceCount = Math.floor(ramAvailable / ramRequired) - 1;
    var newCells = [];
    for(var i = 0; i < instanceCount; i++){
      var newCell = new Cell(hostname, i);
      await ns.sleep(10);
      await ns.exec(
        mainScript,
        hostname,
        1,
        newCell.instanceId);
      newCells.push(newCell);
    }
    
    return newCells;
    
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
    var newCells = await installCell.install(ns.args[1]);
    ns.tprint("Instances installed: ", newCells.length);
  } else if(ns.args.length == 2 && ns.args[0] == "uninstall"){
    await installCell.uninstall(ns.args[1]);
  } else {
    printHelp(ns);
  }
}