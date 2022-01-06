/** @param {NS} ns **/

export async function main(ns){
  await ns.run("database.js", 1, "server");
  await ns.run("bubble-rider.js", 1, "sell");
  await ns.run("bubble-rider.js", 1, "buy");
  await ns.run("reach.js", 1, "manage");
  for(var i = 0; i < 60; i++){
  await ns.run("reach.js", 1, "scan");
    await ns.sleep(60*1000);
  }
}