/** @param {NS} ns **/

import { Crawler } from "crawler.js";
import { DatabaseClient } from "database-client.js";
import { ServerFinder } from "find-server.js";
import { forEach, forEachAsync, GreedyAllocation } from "utils.js";

// A model for an instance of Cell.js running on a server
function Cell(hostname, instanceId){
  
  return {
    hostname,
    instanceId
  };
  
}

// A distribution of jobs that a collection of Cells can handle.
function Schedule(weaken, grow, hack){

  var total = weaken + grow + hack;
  var weakenProportion = weaken / total;
  var growProportion = grow / total;
  var hackProportion = 1 - weakenProportion - growProportion;

  return {
    weaken: weakenProportion,
    grow: growProportion,
    hack: hackProportion
  };

}

// An allocation of work to a given target
function Allocation(target, cells, schedule){
  
  async function assignWork(assignCell){
    
    var totalCells = cells.length;
    var totalWeaken = Math.ceil(totalCells * schedule.weaken);
    var totalGrow = Math.floor(totalCells * schedule.grow);
    var totalHack = Math.floor(totalCells * schedule.hack);
    
    for(var i = 0; i < totalWeaken; i++){
      var cell = cells[i];
      await assignCell(cell, "weaken");
    }
    for(var i = 0; i < totalGrow; i++){
      var cell = cells[totalWeaken + i];
      await assignCell(cell, "grow");
    }
    for(var i = 0; i < totalHack; i++){
      var cell = cells[totalWeaken + totalGrow + i];
      await assignCell(cell, "hack");
    }
    
  }
  
  return {
    assignWork
  };
  
}

// A maintainable collection of cells
function CellPool(){
  
  var hosts = {};
  
  function removeHost(hostname){
    delete hosts[hostname];
  }
  
  function addHost(hostname, cellCount){
    var newCells = [];
    for(var i = 0; i < cellCount; i++){
      newCells.push(new Cell(hostname, i));
    }
    hosts[hostname] = newCells;
  }
  
  function getCells(){
    var allCells = [];
    for(var hostname in hosts){
      var cells = hosts[hostname];
      forEach(cells, function(i, e){
        allCells.push(e);
      });
    }
    return allCells;
  }
  
  return {
    removeHost,
    addHost,
    getCells
  };
  
}

function Allocator(ns){
  
  var weakenSchedule = new Schedule(30,1,1);
  var growSchedule = new Schedule(10,30,1);
  var hackSchedule = new Schedule(10,30,10);
  
  async function updateCellCommand(cell, command){
    ns.tprint(command, " > ", cell); // TODO
  }
  
  async function allocateWork(target, cells){
    
    var currentSecurity = await ns.getServerSecurityLevel(target);
    var targetMaxSecurity = await ns.getServerMinSecurityLevel(target) + 3;
    var weakenStage = currentSecurity > targetMaxSecurity;
    
    var currentMoney = await ns.getServerMoneyAvailable(target);
    var targetMinMoney = await ns.getServerMaxMoney(target) * 0.9;
    var growStage = currentMoney < targetMinMoney;
    
    var schedule;
    if(weakenStage){
      schedule = weakenSchedule;
    } else if(growStage){
      schedule = growSchedule;
    } else {
      schedule = hackSchedule;
    }
    
    return new Allocation(target, cells, schedule);
    
  }
  
  function allocateCells(targets, allCells){
    
    var totalCells = allCells.length;
    var totalTargets = targets.length;
    var cellsForFirst = Math.ceil(totalCells / totalTargets);
    var cellsForRest = Math.floor(totalCells / totalTargets);
    
    var allocations = [];
    forEach(targets, function(i, target){
      
      var firstCell;
      var cellCount;
      if (i=0){
        firstCell = 0;
        cellCount = cellsForFirst;
      } else {
        firstCell = cellsForFirst + cellsForRest*(i-1);
        cellCount = cellsForRest;
      }
      
      allocations.push({
        target,
        cellse: allCells.slice(firstCell, cellCount)
      });
      
    });
    
    return allocations;
    
  }
  
  async function allocate(targets, cellPool){
    var allCells = cellPool.getCells();
    var cellAllocation = allocateCells(targets, allCells);
    await forEachAsync(cellAllocation, async function(i, e){
      var workAllocation = await allocateWork(e.target, e.cells);
      await workAllocation.assignWork(updateCellCommand);
    });
  }
  
  return {
    allocate
  };
  
}

export function CellDirector(ns){
  
  
  return {
    update
  };
  
}

export async function main(ns) {
  var director = new CellDirector(ns, { });
  await director.manage();
}