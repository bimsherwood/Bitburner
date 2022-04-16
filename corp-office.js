/** @param {NS} ns **/

import { EmployeeJobUpgrade, ProportionalUpgrade } from "corp-upgrades.js";

export function OfficeUpgrade(ns, options){

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