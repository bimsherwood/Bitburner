/** @param {NS} ns **/

import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { rootServer } from "./root-server.js";
import { ServerFinder } from "./find-server.js";
import { forEach, forEachAsync } from "./utils.js";
import { getVpsNames, VpsManager } from "./vps.js";

export function Reach(ns, options){
  
  var targetLimit = options.targets;
  var crawler = options.crawler;
  var installer = options.installer;
  var decommission = options.decommission;
  var trace = options.trace;
  
  var serverTargets = { };
  
  function getCurrentServerTarget(server){
    return serverTargets[server] || null;
  }
  
  function setCurrentServerTarget(server, target){
    serverTargets[server] = target;
  }
  
  async function generateSchedule(servers){
    
    var hostFinder = new ServerFinder(ns, {
      hostnames: servers,
      limit: null,
      onlyWithRootAccess: true,
      onlyNotHome: true,
      onlyWithMoney: false,
      onlyNotMine: false
    });
    var bestHosts = await hostFinder.findBestHosts();
    var targetFinder = new ServerFinder(ns, {
      hostnames: servers,
      limit: targetLimit,
      onlyWithRootAccess: true,
      onlyNotHome: true,
      onlyWithMoney: true,
      onlyNotMine: true
    });
    var bestTargets = await targetFinder.findBestTargets();
    
    var schedule = {};
    if(bestTargets.length > 0){
      forEach(bestHosts, function(i, host){
        var correspondingTargetIndex = i % bestTargets.length;
        var correspondingTarget = bestTargets[correspondingTargetIndex];
        schedule[host] = correspondingTarget;
      });
    }
    return schedule;
    
  }
  
  async function tryRootServer(hostname){
    var success = await rootServer(ns, hostname);
    if (success){
      await trace("> Rooted server " + hostname);
    }
  }
  
  async function tryInstall(hostname, target){
    var canInstall =
      await ns.hasRootAccess(hostname) &&
      await ns.hasRootAccess(target) &&
      hostname != "home";
    if (canInstall){
      await trace("> Updating: " + hostname + " targeting " + target);
      await installer.installMax(hostname, target);
      setCurrentServerTarget(hostname, target);
    } else {
      await trace("! Cannot update: " + hostname);
    }
  }
  
  async function deployEverywhere(forceReinstall){
    
    // Root servers
    var servers = await crawler.crawl();
    await forEachAsync(servers, async function(i, e){
      await tryRootServer(e);
    });
    
    // Schedule hosts and targets
    var schedule = await generateSchedule(servers);
    for(var server in schedule){
      var newTarget = schedule[server];
      var currentTarget = getCurrentServerTarget(server);
      var reinstall =
        newTarget != currentTarget ||
        forceReinstall;
      if (reinstall && newTarget){
        await tryInstall(server, newTarget);
        await ns.sleep(500);
      } else {
        trace("> No update for " + server);
      }
    }

    trace("> Deployment complete.");
    
  }
  
  async function removeEverywhere(){
    var servers = await crawler.crawl();
    await forEachAsync(servers, async function(i, e){
      if(e != "home"){
        await installer.uninstall(e);
      }
    });
  }
  
  async function upgradeVps(){
    var vpsHosts = await getVpsNames();
    var manager = new VpsManager(ns, {
      hostnames: vpsHosts,
      decommission: decommission,
      commission: function(){ },
      trace: trace
    });
    await manager.upgrade();
    trace("> VPS upgrade complete.");
  }
  
  async function manage(){
    var upgradePeriod = 60*1000;
    for(;;){
      await upgradeVps();
      await deployEverywhere(false);
      await ns.sleep(upgradePeriod);
    }
  }
  
  return {
    deployEverywhere,
    manage,
    removeEverywhere
  };
  
}

export async function main(ns) {
  
  var install =
    ns.args.length == 2 &&
    ns.args[0] == "install";
  var manage =
    ns.args.length == 2 &&
    ns.args[0] == "manage";
  var uninstall = 
    ns.args.length == 1 &&
    ns.args[0] == "uninstall";
  var target = ns.args.length >= 2 ? ns.args[1] : null;
  
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var installer = new InstallThief(ns);
  var reach = new Reach(ns, {
    targets: target,
    crawler: crawler,
    installer: installer,
    decommission: async function(hostname){ await ns.killall(hostname); },
    trace: manage ? ns.print : ns.tprint
  });
  
  if (install){
    await reach.deployEverywhere(true);
    ns.tprint("Done.");
  } else if (manage){
    await reach.manage();
  } else if (uninstall){
    await reach.removeEverywhere();
    ns.tprint("Done.");
  } else {
    ns.tprint("Usage:");
    ns.tprint("  reach.js install <target limit> <host>");
    ns.tprint("  reach.js manage <target limit>");
    ns.tprint("  reach.js uninstall");
  }
  
}