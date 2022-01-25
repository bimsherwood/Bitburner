/** @param {NS} ns **/

import { forEachAsync } from "utils.js";

function Upgradeable(options){
  
  var getSize = options.getSize;
  var getUpgradeCost = options.getUpgradeCost;
  var upgrade = options.upgrade;
  var proportion = options.proportion;
  
  return {
    getSize,
    getUpgradeCost,
    upgrade, // returns false if the upgrade failed
    proportion
  };
  
}

export function ProportionalUpgrade(ns, options){
  
  var subUpgrades = options.subUpgrades;
  var proportion = options.proportion;

  async function getNextUpgrade(){

    var totalPoints = 0;
    var totalSize = 0;
    var scoredUpgrades = [];
    await forEachAsync(subUpgrades, async function(i, e){
      var points = e.proportion;
      var size = await e.getSize();
      totalPoints += points;
      totalSize += size;
      scoredUpgrades.push({
        size,
        points,
        upgrade: e
      });
    });

    var fallingShort = scoredUpgrades
      .map(function(u){
        var targetProportion = u.points / totalPoints;
        var actualProportion = u.size / totalSize;
        var priority = targetProportion - actualProportion;
        return {
          priority,
          upgrade: u.upgrade
        };
      })
      .sort(function(a, b){
        return b.priority - a.priority;
      });

    return fallingShort[0].upgrade;

  }

  async function getSize(){
    var totalSize = 0;
    await forEachAsync(subUpgrades, async function(i, e){
      totalSize += await e.getSize();
    });
    return totalSize;
  }

  async function getUpgradeCost(){
    var nextUpgrade = await getNextUpgrade();
    return await nextUpgrade.getUpgradeCost();
  }

  async function upgrade(){
    var nextUpgrade = await getNextUpgrade();
    return await nextUpgrade.upgrade();
  }
  
  return new Upgradeable({
    getSize,
    getUpgradeCost,
    upgrade,
    proportion
  });
  
}

export function EmployeeJobUpgrade(ns, options){

  var division = options.division;
  var city = options.city;
  var job = options.job;
  var proportion = options.proportion;

  async function getSize(){
    var office = await ns.corporation.getOffice(division, city);
    var employeeNames = office.employees;
    var employees = [];
    await forEachAsync(employeeNames, async function(i, e){
      employees.push(await ns.corporation.getEmployee(division, city, e));
    });
    var employeesInJob = employees.filter(function(e){
      return e.pos == job;
    });
    return employeesInJob.length;
  }

  async function getUpgradeCost(){
    var office = await ns.corporation.getOffice(division, city);
    var capacity = office.size;
    var hires = office.employees.length;
    if (capacity <= hires){
      return await ns.corporation.getOfficeSizeUpgradeCost(division, city, 1);
    } else {
      return 0;
    }
  }

  async function upgrade(){
    var office = await ns.corporation.getOffice(division, city);
    var capacity = office.size;
    var hires = office.employees.length;
    if (capacity <= hires){
      await ns.corporation.upgradeOfficeSize(division, city, 1);
    }
    var newEmployee = await ns.corporation.hireEmployee(division, city);
    if(!newEmployee){
      return false;
    }
    await ns.corporation.assignJob(division, city, newEmployee.name, job);
    return true;
  }

  return new Upgradeable({
    getSize,
    getUpgradeCost,
    upgrade,
    proportion
  });

}