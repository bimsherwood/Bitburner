/** @param {NS} ns **/

import { forEach } from "utils.js";
import { Crawler } from "crawler.js";
import { ServerFinder } from "find-server.js";

export async function main(ns){
  
  // List servers
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var hostnames = await crawler.crawl();
  
  var targetFinder = new ServerFinder(ns, {
    hostnames: hostnames,
    limit: 1000,
    onlyWithRootAccess: true,
    onlyWithMoney: true,
    onlyNotMine: true,
    onlyNotHome: true,
    onlyLowSecurity: true
  });
  var targets = await targetFinder.findBestTargets();
  
  forEach(targets, function(i, server){
    ns.tprint(server);
  });
  
}