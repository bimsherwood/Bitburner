/** @param {NS} ns **/

import { safeLoop, forEach } from "utils.js";
import { EmployeeJobUpgrade, ProportionalUpgrade } from "corp-upgrades.js";

function OfficeUpgrade(ns, options){

  var division = options.division;
  var city = options.city;
  var proportion = options.proportion;

  var employeeOperationsUpgrade = new EmployeeJobUpgrade(ns, {
    division,
    city,
    job: "Operations",
    proportion: 8
  });
  var employeeEngineerUpgrade = new EmployeeJobUpgrade(ns, {
    division,
    city,
    job: "Engineer",
    proportion: 4
  });
  var employeeBusinessUpgrade = new EmployeeJobUpgrade(ns, {
    division,
    city,
    job: "Business",
    proportion: 2
  });
  var employeeManagementUpgrade = new EmployeeJobUpgrade(ns, {
    division,
    city,
    job: "Management",
    proportion: 1
  });

  return new ProportionalUpgrade(ns, {
    subUpgrades: [
      employeeOperationsUpgrade,
      employeeEngineerUpgrade,
      employeeBusinessUpgrade,
      employeeManagementUpgrade],
    proportion
  });

}

export function AgricultureStrategy (ns, options){
  
  var divisionName = options.divisionName;
  var cities = options.cities;

  var officeUpgrades = [];
  forEach(cities, function(i, e){
    var officeUpgrade = new OfficeUpgrade(ns, {
      division: divisionName,
      city: e,
      proportion: 1
    });
    officeUpgrades.push(officeUpgrade);
  });

  var agricultureUpgrade = new ProportionalUpgrade(ns, {
    subUpgrades: officeUpgrades,
    proportion: 1
  });

  async function employ() {
    var corporation = await ns.corporation.getCorporation();
    var cost = await agricultureUpgrade.getUpgradeCost();
    var funds = corporation.funds;
    if (cost <= funds){
      await agricultureUpgrade.upgrade();
    }
  }
  
  return {
    employ
  };
  
}

export async function main(ns){
  
  var strategy = new AgricultureStrategy(ns, {
    divisionName: "BimGronomy",
    cities: ["Aevum", "Sector-12"]
  });
  await safeLoop(ns, async function(){
    await strategy.employ();
    await ns.sleep(10000);
  });
  
}