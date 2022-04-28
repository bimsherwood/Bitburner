/** @param {NS} ns **/

import { forEach } from "utils.js";
import { ProportionalUpgrade } from "corp-upgrades.js";
import { OfficeUpgrade } from "corp-office.js";

export function SoftwareUpgrade (ns, options){
  
  var divisionName = options.divisionName;
  var cities = options.cities;
  var proportion = options.proportion;

  var officeUpgrades = [];
  forEach(cities, function(i, e){
    var officeUpgrade = new OfficeUpgrade(ns, {
      division: divisionName,
      city: e,
      proportion: 1
    });
    officeUpgrades.push(officeUpgrade);
  });

  return new ProportionalUpgrade(ns, {
    subUpgrades: officeUpgrades,
    proportion: 1
  });
  
}
