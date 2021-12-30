/** @param {NS} ns **/

import { forEachAsync } from "./utils.js";

export function portPoppers(ns){
  return [
    ns.brutessh,
    ns.ftpcrack
  ];
}

export async function rootServer(ns, hostname){
  var poppers = portPoppers(ns);
  await forEachAsync(poppers, async function(i, e){
    await e(hostname);
  });
  await ns.nuke(hostname);
}

function printHelp(ns){
  ns.print("Usage:");
  ns.print("  root-server.js <hostname>");
}

export async function main(ns) {
  if (ns.args.length == 1){
    await rootServer(ns, ns.args[0]);
  } else {
    printHelp(ns);
  }
}