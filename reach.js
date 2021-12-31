/** @param {NS} ns **/

import { Cache } from "./cache.js";
import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { rootServer } from "./root-server.js";
import { findBestTargets } from "./find-target.js";
import { forEachAsync } from "./utils.js";

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
  
  function getServerTarget(server){
    return serverTargets[server] || null;
  }
  
  function setServerTarget(server, target){
    serverTargets[server] = target;
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
      ns.tprint("Installing on server " + hostname);
      await install(hostname, target);
      setServerTarget(hostname, target);
    }
  }
  
  async function discover(forceReinstall){
    
    // Find servers
    var servers = await crawler.crawl();
    
    // Root servers
    await forEachAsync(servers, async function(i, e){
      await maybeRootServer(e);
    });
    
    // Identify a target
    var bestTargets = await findBestTargets(ns);
    if(bestTargets.length == 0){
      ns.tprint("No suitable target found.");
      return;
    }
    var target = bestTargets[0].hostname;
    ns.tprint("Targeting ", target);
    
    // Install/re-install
    await forEachAsync(servers, async function(i, e){
      var currentTarget = getServerTarget(e);
      var resinstall =
        target != currentTarget ||
        forceReinstall;
      if (resinstall){
        await tryUpdateServer(e, target, false);
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
  
}