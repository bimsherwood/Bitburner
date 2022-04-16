/** @param {NS} ns **/

import { safeLoop } from "utils.js";
import { ProportionalUpgrade } from "corp-upgrades.js";
import { AgricultureUpgrade } from "corp-strat-ag.js";
import { SoftwareUpgrade } from "corp-strat-soft.js";
import { ChemicalUpgrade } from "corp-strat-chem.js";

export function CorporateStrategy(ns){
  
  var cities = [
    "Aevum",
    "Sector-12",
    "Chongqing",
    "New Tokyo",
    "Ishima",
    "Volhaven"];
  
  var agriculture = new AgricultureUpgrade(ns, {
    divisionName: "BimGronomy",
    cities
  });
  var software = new SoftwareUpgrade(ns, {
    divisionName: "BimTech",
    cities
  });
  var chemical = new ChemicalUpgrade(ns, {
    divisionName: "BimIcal",
    cities
  });
  
  var strategy = new ProportionalUpgrade(ns, {
    subUpgrades: [
      agriculture,
      software,
      chemical],
    proportion: 1
  });
  
  async function employ() {
    var corporation = await ns.corporation.getCorporation();
    var cost = await strategy.getUpgradeCost();
    var funds = corporation.funds;
    if (cost <= funds){
      return await strategy.upgrade();
    } else {
      return false;
    }
  }
  
  return {
    employ
  };
  
}

export async function main(ns){
  
  var strategy = CorporateStrategy(ns);
  await safeLoop(ns, async function(){
    while(await strategy.employ()){
      await ns.sleep(500);
    }
    await ns.sleep(6000);
  });
  
}