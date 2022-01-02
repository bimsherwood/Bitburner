/** @param {NS} ns **/

import { Cache } from "./cache.js";
import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { rootServer } from "./root-server.js";
import { ServerFinder } from "./find-server.js";
import { forEach, forEachAsync } from "./utils.js";

export function Reach(ns, crawler, install){
  
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
  
  function pairHostsAndTargets(bestHosts, bestTargets){
    var schedule = {};
    forEach(bestHosts, function(i, host){
      var correspondingTargetIndex = i % bestTargets.length;
      var correspondingTarget = bestTargets[correspondingTargetIndex];
      schedule[host] = correspondingTarget;
    });
    return schedule;
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
      limit: 10,
      onlyWithRootAccess: true,
      onlyNotHome: true,
      onlyWithMoney: true,
      onlyNotMine: true
    });
    var bestTargets = await targetFinder.findBestTargets();
    if(bestTargets.length == 0){
      return null;
    }
    return pairHostsAndTargets(bestHosts, bestTargets);
  }
  
  async function tryRootServer(hostname){
    var success = await rootServer(ns, hostname);
    if (success){
      ns.tprint("Rooted server " + hostname);
    }
  }
  
  async function tryInstall(hostname, target){
    var canInstall =
      await ns.hasRootAccess(hostname) &&
      hostname != "home";
    if (canInstall){
      ns.tprint("Updating: ", hostname, " targeting " + target);
      await install(hostname, target);
      setCurrentServerTarget(hostname, target);
    }
  }
  
  async function deployOn(host){
    var servers = await crawler.crawl();
    var schedule = await generateSchedule(servers);
    if(schedule == null){
      ns.tprint("No suitable hosts and/or targets found.");
      return;
    }
    var target = schedule[host];
    await tryRootServer(host);
    await tryRootServer(target);
    await tryInstall(host, target);
  }
  
  async function deploy(forceReinstall){
    
    // Root servers
    var servers = await crawler.crawl();
    await forEachAsync(servers, async function(i, e){
      await tryRootServer(e);
    });
    
    // Find best hosts and targets
    var schedule = await generateSchedule(servers);
    if(schedule == null){
      ns.tprint("No suitable hosts and/or targets found.");
      return;
    }
    
    // Schedule hosts and targets
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
  
  return {
    init,
    deploy,
    deployOn
  };
  
}

export async function main(ns) {
  
  var reinstallLazy =
    ns.args.length == 1 &&
    ns.args[0] == "scan";
  var reinstallForce =
    ns.args.length == 2 &&
    ns.args[0] == "scan" &&
    ns.args[1] == "--force";
  var scan = reinstallLazy || reinstallForce;
  var install =
    ns.args.length == 2 &&
    ns.args[0] == "install";
  
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var installer = new InstallThief(ns);
  var reach = new Reach(ns, crawler, installer.installMax);
  
  if(scan){
    await reach.init();
    await reach.deploy(reinstallForce);
    ns.tprint("Done.");
  } else if (install){
    var host = ns.args[1];
    await reach.init();
    await reach.deployOn(host);
    ns.tprint("Done.");
  } else {
    ns.tprint("Usage:");
    ns.tprint("  reach.js scan [--force]");
    ns.tprint("  reach.js install <host>");
  }
  
}