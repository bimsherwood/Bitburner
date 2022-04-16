/** @param {NS} ns **/

//import { forEach } from "utils.js";
import { StorageUpgrade } from "corp-upgrades.js";

export async function main(ns) {
  
  var upgrade = StorageUpgrade(ns, {
    division: "BimRock",
    city: "Sector-12",
    proportion: 1
  });
  
  await ns.tprint("sdsd");
  await ns.tprint(await upgrade.getSize());
  await ns.tprint(await upgrade.getUpgradeCost());
  
  if(ns.args.length > 0){
    await ns.tprint(await upgrade.upgrade());
  }
  
}