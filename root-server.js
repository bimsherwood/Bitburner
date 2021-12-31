/** @param {NS} ns **/

import { forEachAsync } from "./utils.js";

export async function portPoppers(ns){
  
  var possiblePoppers = [
    { bin: "FTPCrack.exe", invoke: ns.ftpcrack },
    { bin: "BruteSSH.exe", invoke: ns.brutessh },
    { bin: "relaySMTP.exe", invoke: ns.relaysmtp },
    { bin: "HTTPWorm.exe", invoke: ns.httpworm },
    { bin: "SQLInject.exe", invoke: ns.sqlinject }
  ];
  
  var installedPopppers = [];
  await forEachAsync(possiblePoppers, async function(i, e){
    if (await ns.fileExists(e.bin, "home")){
      installedPopppers.push(e);
    }
  });
  
  return installedPopppers;
  
}

export async function isVulnerable(ns, hostname) {
  var poppers = await portPoppers(ns);
  var myLevel = await ns.getHackingLevel();
  var requiredLevel = await ns.getServerRequiredHackingLevel(hostname);
  var portsRequired = await ns.getServerNumPortsRequired(hostname);
  return myLevel >= requiredLevel && poppers.length >= portsRequired;
}

export async function rootServer(ns, hostname){
  var hasRootAccess = await ns.hasRootAccess(hostname);
  var vulnerable = await isVulnerable(ns, hostname);
  if(!hasRootAccess && vulnerable){
    var poppers = await portPoppers(ns);
    await forEachAsync(poppers, async function(i, e){
      await e.invoke(hostname);
    });
    await ns.nuke(hostname);
    return true;
  } else {
    return false;
  }
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  root-server.js <hostname>");
}

export async function main(ns) {
  if (ns.args.length == 1){
    if(await rootServer(ns, ns.args[0])){
      ns.tprint("Success");
    } else {
      ns.tprint("Failed");
    }
  } else {
    printHelp(ns);
  }
}