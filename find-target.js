/** @param {NS} ns **/

import { forEachAsync } from "./utils.js";
import { Crawler } from "./crawler.js";

async function profileServer(ns, hostname){
  return {
    hostname: hostname,
    hasRootAccess: await ns.hasRootAccess(hostname),
    money: await ns.getServerMoneyAvailable(hostname)
  }
}

function compareProfiles(profilea, profileb){
  return profileb.money - profilea.money;
}

function hasRootAccess(server){
  return server.hasRootAccess;
}

async function notMine(ns){
  var myServers = await ns.getPurchasedServers();
  myServers.push("home");
  return function(server){
    return myServers.indexOf(server.hostname) < 0;
  }
}

function printProfile(ns, profile){
  ns.tprint(profile.hostname, ": ", profile.money);
}

// Lists the rooted servers in order of available money descending
export async function findBestTargets(ns){
  
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var servers = await crawler.crawl();
  
  var profiles = [];
  await forEachAsync(servers, async function(i, e){
    profiles.push(await profileServer(ns, e));
  });
  
  var targets = profiles
    .filter(hasRootAccess)
    .filter(await notMine(ns))
    .sort(compareProfiles);
  
  return targets
  
}

export async function main(ns){
  var targets = await findBestTargets(ns);
  await forEachAsync(targets, async function(i, e){
    await printProfile(ns, e);
  });
}