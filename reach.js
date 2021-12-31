/** @param {NS} ns **/

import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { rootServer } from "./root-server.js";
import { findBestTargets } from "./find-target.js";
import { forEachAsync } from "./utils.js";

function Reach(ns, crawler, install){
  
  async function maybeRootServer(hostname){
    var success = await rootServer(ns, hostname);
    if (success){
      ns.tprint("Rooted server " + hostname);
    }
  }
  
  async function updateServer(hostname, target){
    var canInstall =
      await ns.hasRootAccess(hostname) &&
      hostname != "home";
    if (canInstall){
      ns.tprint("Installing on server " + hostname);
      await install(hostname, target);
    }
  }
  
  async function discover(){
    var servers = await crawler.crawl();
    await forEachAsync(servers, async function(i, e){
      await maybeRootServer(e);
    });
    var bestTargets = await findBestTargets(ns);
    if(bestTargets.length > 0){
      var target = bestTargets[0].hostname;
      ns.tprint("Targeting ", target);
      await forEachAsync(servers, async function(i, e){
        await updateServer(e, target, false);
      });
    } else {
      ns.tprint("No suitable target found.");
    }
  }
  
  return {
    discover
  };
  
}

export async function main(ns) {
  
  var installer = new InstallThief(ns);
  var crawler = new Crawler(ns, {
    resultLimit: 100,
    rootHost: "home"
  });
  var reach = new Reach(ns, crawler, installer.installMax);
  await reach.discover();
  
}