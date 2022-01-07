/** @param {NS} ns **/

import { Cache } from "./cache.js";
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
  
  var serverTargets = null;
  
  async function init(){
    var cacheCtxt = new Cache(ns, "reach");
    var cache = await cacheCtxt.open();
    serverTargets = cache.load("server-targets");
    if (serverTargets == null){
      serverTargets = { };
      cache.save("server-targets", serverTargets);
    }
  }
  
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
      await trace("Rooted server " + hostname);
    }
  }
  
  async function tryInstall(hostname, target){
    var canInstall =
      await ns.hasRootAccess(hostname) &&
      await ns.hasRootAccess(target) &&
      hostname != "home";
    if (canInstall){
      await trace("Updating: " + hostname + " targeting " + target);
      await installer.installMax(hostname, target);
      setCurrentServerTarget(hostname, target);
    }
  }
  
  async function deployOn(host){
    var servers = await crawler.crawl();
    var currentTarget = getCurrentServerTarget(host);
    var target;
    if(currentTarget != null){
      target = currentTarget;
    } else {
      var newSchedule = await generateSchedule(servers);
      target = newSchedule[host];
      await trace("New server " + host);
      await trace(newSchedule);
    }
    if(target != null){
      await tryInstall(host, target);
    } else {
      await trace("No target allocated to ", host);
      return;
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
    await forEachAsync(servers, async function(i, e){
      var newTarget = schedule[e];
      var currentTarget = getCurrentServerTarget(e);
      var reinstall =
        newTarget != currentTarget ||
        forceReinstall;
      if (reinstall && newTarget){
        await tryInstall(e, newTarget);
      }
    });
    
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
      commission: deployOn,
      trace: trace
    });
    await manager.upgrade();
  }
  
  async function manage(){
    var upgradePeriod = 60*1000;
    var scanPeriod = 5*upgradePeriod;
    for(;;){
      await deployEverywhere(false);
      for(var i = 0; i < scanPeriod; i += upgradePeriod){
        await upgradeVps();
        await ns.sleep(upgradePeriod);
      }
    }
  }
  
  return {
    init,
    deployEverywhere,
    deployOn,
    manage,
    removeEverywhere
  };
  
}

export async function main(ns) {
  
  var scanLazy =
    ns.args.length == 2 &&
    ns.args[0] == "scan" &&
    ns.args[1] != "--force";
  var scanForce =
    ns.args.length == 3 &&
    ns.args[0] == "scan" &&
    ns.args[2] == "--force";
  var scan = scanLazy || scanForce;
  var install =
    ns.args.length == 3 &&
    ns.args[0] == "install";
  var manage =
    ns.args.length == 2 &&
    ns.args[0] == "manage";
  var uninstall = 
    ns.args.length == 1 &&
    ns.args[0] == "uninstall";
  
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var installer = new InstallThief(ns);
  var reach = new Reach(ns, {
    targets: ns.args[1],
    crawler: crawler,
    installer: installer,
    decommission: async function(hostname){ await ns.killall(hostname); },
    trace: manage ? ns.print : ns.tprint
  });
  
  if(scan){
    await reach.init();
    await reach.deployEverywhere(scanForce);
    ns.tprint("Done.");
  } else if (install){
    var host = ns.args[2];
    await reach.init();
    await reach.deployOn(host);
    ns.tprint("Done.");
  } else if (manage){
    await reach.init();
    await reach.manage();
  } else if (uninstall){
    await reach.init();
    await reach.removeEverywhere();
  } else {
    ns.tprint("Usage:");
    ns.tprint("  reach.js scan <target limit> [--force]");
    ns.tprint("  reach.js install <target limit> <host>");
    ns.tprint("  reach.js manage <target limit>");
    ns.tprint("  reach.js uninstall");
  }
  
}