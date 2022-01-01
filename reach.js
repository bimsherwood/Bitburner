/** @param {NS} ns **/

import { Cache } from "./cache.js";
import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { rootServer } from "./root-server.js";
import { findBestTargets } from "./find-target.js";
import { forEach, forEachAsync } from "./utils.js";

function Reach(ns, crawler, install){
  
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
  
  async function findBestHosts(){
    
    var servers = await crawler.crawl();
    
    var serverPower = [];
    await forEachAsync(servers, async function(i, e){
      serverPower.push({
        hostname: e,
        power: await ns.getServerRam(e)
      });
    });
    
    var serversByPowerDesc = serverPower.sort(function(a, b){
      return b.power - a.power;
    });
    
    return serversByPowerDesc
      .map(function(serverPower){
        return serverPower.hostname;
      });
  }
  
  function pairHostsAndTargets(hostsByPowerDesc, targetsByMoneyDesc){
    var schedule = [];
    forEach(hostsByPowerDesc, function(i, host){
      var correspondingTargetIndex = i % targetsByMoneyDesc.length;
      var correspondingTarget =
        targetsByMoneyDesc[correspondingTargetIndex].hostname;
      schedule.push({
        host: host,
        target: correspondingTarget
      });
    });
    return schedule;
  }
  
  async function maybeRootServer(hostname){
    var success = await rootServer(ns, hostname);
    if (success){
      ns.tprint("Rooted server " + hostname);
    }
  }
  
  async function tryUpdateServer(hostname, target){
    var canInstall =
      await ns.hasRootAccess(hostname) &&
      hostname != "home";
    if (canInstall){
      ns.tprint("Updating: ", hostname, " targeting " + target);
      await install(hostname, target);
      setCurrentServerTarget(hostname, target);
    }
  }
  
  async function discover(forceReinstall){
    
    // Find servers
    var bestHosts = await findBestHosts();
    
    // Root servers
    await forEachAsync(bestHosts, async function(i, e){
      await maybeRootServer(e);
    });
    
    // Find targets
    var bestTargets = await findBestTargets(ns);
    if(bestTargets.length == 0){
      ns.tprint("No suitable target found.");
      return;
    }
    
    // Schedule hosts and targets
    var schedule = pairHostsAndTargets(bestHosts, bestTargets);
    await forEachAsync(schedule, async function(i, e){
      var currentTarget = getCurrentServerTarget(e.host);
      var resinstall =
        e.target != currentTarget ||
        forceReinstall;
      if (resinstall){
        await tryUpdateServer(e.host, e.target);
      }
    });
    
  }
  
  return {
    init,
    discover
  };
  
}

export async function main(ns) {
  
  var forceReinstall;
  if (ns.args.length == 0){
    forceReinstall = false;
  } else if(ns.args[0] == "--reinstall") {
    forceReinstall = true;
  } else {
    ns.tprint("Usage:");
    ns.tprint("  reach.js");
    ns.tprint("  reach.js --reinstall");
    return;
  }
  
  var installer = new InstallThief(ns);
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var reach = new Reach(ns, crawler, installer.installMax);
  await reach.init();
  await reach.discover(forceReinstall);
  ns.tprint("Done.");
  
}