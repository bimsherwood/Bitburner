/** @param {NS} ns **/

import { Crawler } from "crawler.js";
import { DatabaseClient } from "database-client.js";
import { ServerFinder } from "find-server.js";
import { forEach, forEachAsync, GreedyAllocation } from "utils.js";

export function CellDirector(ns, options){
  
  var effortDistribution = [64,32,16,8,4,2,1,1];
  var database = options.database;
  var crawler = options.crawler;
  
  function sum(array){
    return array.reduce(function (a, b) { return a + b; }, 0);
  }
  
  function scaleDistribution(reference, scale, length){
    
    // Truncate and scale the 
    var distTruncated = reference.slice(0, length);
    var referenceTotal = sum(distTruncated);
    var dist = distTruncated
      .map(function(x){
        return x * scale / referenceTotal;
      });
    
    // If the distribution is too short, add some zeros
    var extension = [];
    for(var i = 0; i < length - dist.length; i++){
      extension.push(0);
    }
    
    return dist.concat(extension);
  }
  
  async function setWork(target, cells){
    var cellStatus = "continuous";
    var cellInstruction = {
      operation: "weaken",
      target: target
    };
    await forEachAsync(cells, async function(i, e){
      var cellGlobalName = e.hostname + "|" + e.instance;
      await database.write(cellGlobalName + "-status", cellStatus);
      await database.write(cellGlobalName + "-instruction", cellInstruction);
    });
  }
  
  async function generateSchedule(cells, servers){
    
    // Identify some targets
    var targetFinder = new ServerFinder(ns, {
      hostnames: servers,
      limit: effortDistribution.length,
      onlyWithRootAccess: true,
      onlyNotHome: true,
      onlyWithMoney: true,
      onlyNotMine: true
    });
    var bestTargets = await targetFinder.findBestTargets();
    
    // Allocate threads to targets
    var totalThreads = sum(cells.map(function(cell){ return cell.threads; }));
    var threadDistribution = scaleDistribution(
        effortDistribution,
        totalThreads,
        bestTargets.length);
    
    // Allocate worker Cells to targets
    var targetThreads = [];
    for(var i = 0; i < threadDistribution.length; i++){
      targetThreads.push({
        threads: threadDistribution[i],
        target: bestTargets[i]
      });
    }
    var allocator = new GreedyAllocation(
      targetThreads,
      function(targetThread){ return targetThread.threads; },
      cells,
      function(cell){ return cell.threads; });
    var greedyAllocation = allocator.allocate();
    
    // Return the schedule
    var schedule = [];
    forEach(greedyAllocation.allocations, function(i, e){
      schedule.push({
        target: e[0].target,
        cells: e[1]
      });
    });
    return schedule;
    
  }
  
  async function manage(){
    var cells = await database.read("cells");
    var servers = await crawler.crawl();
    if (cells != null){
      var schedule = await generateSchedule(cells, servers);
      await forEachAsync(schedule, async function(i, e){
        await setWork(e.target, e.cells);
      });
    } else {
      ns.tprint("No Cells are registered.");
    }
  }
  
  return {
    manage
  }
  
}

export async function main(ns) {
  var databaseName = "cell";
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var database = new DatabaseClient(ns, databaseName);
  var director = new CellDirector(ns, {
    database,
    crawler
  });
  await director.manage();
}