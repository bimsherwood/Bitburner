/** @param {NS} ns **/

import { Crawler } from "crawler.js";
import { ServerFinder } from "find-server.js";
import { Cell, InstallCell } from "install-cell.js";
import { rootServer } from "root-server.js";
import { forEach, forEachAsync, safeLoop } from "utils.js";
import { getVpsNames, VpsManager } from "vps.js";

var _bioticState;

function getBioticState(){
  if(typeof(_bioticState) === "undefined"){
    _bioticState = {};
  }
  if(typeof(_bioticState.cellPool) === "undefined"){
    _bioticState.cellPool = new CellPool();
  }
  return _bioticState;
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
      await assignCell(cell, "weaken", target);
    }
    for(var i = 0; i < totalGrow; i++){
      var cell = cells[totalWeaken + i];
      await assignCell(cell, "grow", target);
    }
    for(var i = 0; i < totalHack; i++){
      var cell = cells[totalWeaken + totalGrow + i];
      await assignCell(cell, "hack", target);
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
  
  function addHost(hostname, cells){
    hosts[hostname] = cells;
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
  
  function clear(){
    hosts = {};
  }
  
  return {
    removeHost,
    addHost,
    getCells,
    clear
  };
  
}

function Allocator(ns){
  
  var weakenSchedule = new Schedule(1,0,0);
  var growSchedule = new Schedule(1,3,0);
  var hackSchedule = new Schedule(1,1,1);
  
  async function updateCellCommand(cell, command, target){
    
    async function runUpdate(){
      await ns.exec(
        "cell.js",
        cell.hostname,
        1,
        cell.instanceId,
        command,
        target);
    }
    
    async function updateStillRunning(){
      return await ns.isRunning(
        "cell.js",
        cell.hostname,
        cell.instanceId,
        command,
        target)
    }
    
    await runUpdate();
    await ns.sleep(10);
    while(await updateStillRunning()){
      await ns.sleep(10);
    }
    
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
      ns.tprint("Weakening ", target, " (", cells.length, ")");
      schedule = weakenSchedule;
    } else if(growStage){
      ns.tprint("Growing ", target, " (", cells.length, ")");
      schedule = growSchedule;
    } else {
      ns.tprint("Hacking ", target, " (", cells.length, ")");
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
      var stopCell;
      if (i==0){
        firstCell = 0;
        stopCell = cellsForFirst;
      } else {
        firstCell = cellsForFirst + cellsForRest*(i-1);
        stopCell = firstCell + cellsForRest;
      }
      
      allocations.push({
        target,
        cells: allCells.slice(firstCell, stopCell)
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
  
  async function spam(target, cellPool){
    var allCells = cellPool.getCells();
    var cellAllocation = allocateCells([target], allCells)[0];
    var workAllocation = new Allocation(
      cellAllocation.target,
      cellAllocation.cells,
      weakenSchedule);
    await workAllocation.assignWork(updateCellCommand);
  }
  
  return {
    allocate,
    spam
  };
  
}

function CellManager(ns, options){
  
  var installer = options.installer;
  var crawler = options.crawler;
  
  var cellPool = getBioticState().cellPool;
  
  async function uninstallOn(hostname){
    await installer.uninstall(hostname);
  }
  
  async function installOn(hostname){
    var newCells = await installer.install(hostname);
    cellPool.addHost(hostname, newCells);
  }
  
  async function install(hostname){
    
    // Identify hosts
    var allServers = await crawler.crawl();
    var hosts = [];
    await forEachAsync(allServers, async function(i, hostname){
      var isHome = hostname == "home";
      var isRooted = await ns.hasRootAccess(hostname);
      if(isRooted && !isHome){
        hosts.push(hostname);
      }
    });
    
    // Install on hosts
    cellPool.clear();
    await forEachAsync(hosts, async function(i, hostname){
      await installOn(hostname);
    });
    
  }
  
  return {
    uninstallOn,
    installOn,
    install
  };
  
}

function Biotic (ns, options){
  
  var trace = options.trace;
  var cellManager = options.cellManager;
  var crawler = options.crawler;
  var allocator = options.allocator;
  var targetLimit = options.targetLimit;
  var vpsUpgrade = options.vpsUpgrade;
  var spam = options.spam;
  
  var bioticState = getBioticState();
  var cellPool = bioticState.cellPool;
  
  var upgradePeriod = 5*60*1000;
  
  async function rootServers(){
    var allServers = await crawler.crawl();
    await forEachAsync(allServers, async function(i, hostname){
      var success = await rootServer(ns, hostname);
      if (success){
        await ns.sleep(1000);
        await cellManager.installOn(hostname);
        await trace("Rooted server " + hostname);
      }
    });
  }
  
  async function upgradeVps(){
    var vpsHosts = await getVpsNames();
    var manager = new VpsManager(ns, {
      hostnames: vpsHosts,
      decommission: cellManager.uninstallOn,
      commission: cellManager.installOn,
      trace: trace
    });
    await manager.upgrade();
  }
  
  async function allocateWork(){
    var targets;
    if(!spam){
      var allServers = await crawler.crawl();
      var targetFinder = new ServerFinder(ns, {
        hostnames: allServers,
        limit: targetLimit,
        onlyWithRootAccess: true,
        onlyWithMoney: true,
        onlyNotMine: true,
        onlyNotHome: true
      });
      var targets = await targetFinder.findBestTargets();
      await allocator.allocate(targets, cellPool);
    } else {
      await allocator.spam("n00dles", cellPool);
    }
  }
  
  async function manage(){
    await cellManager.install();
    await safeLoop(ns, async function (){
      await rootServers();
      if(vpsUpgrade){
        await upgradeVps();
      }
      await allocateWork();
      await ns.sleep(upgradePeriod);
    });
  }
  
  return {
    manage
  };
  
}

export async function main(ns) {
  
  var targetLimit;
  if(ns.args.length > 0){
    targetLimit = ns.args[0];
  } else {
    targetLimit = 1;
  }
  
  var vpsUpgrade = ns.args.indexOf("--no-vps-upgrade") < 0;
  var spam = ns.args.indexOf("--spam") >= 0;
  
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var allocator = new Allocator(ns);
  var installer = new InstallCell(ns);
  var cellManager = new CellManager(ns, {
    installer: installer,
    crawler: crawler
  });
  var biotic = new Biotic(ns, {
    trace: ns.tprint,
    cellManager: cellManager,
    crawler: crawler,
    allocator: allocator,
    targetLimit: targetLimit,
    vpsUpgrade: vpsUpgrade,
    spam: spam
  });
  await biotic.manage();
  
}