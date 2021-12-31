/** @param {NS} ns **/

import { forEach, forEachAsync } from "./utils.js";
import { Crawler } from "./crawler.js";

export async function listFiles(ns, hostname, pattern){
  var files = await ns.ls(hostname, pattern);
  forEach(files, function(i, e){
    ns.tprint(hostname + "/" + e);
  });
}

export async function main(ns){
  var pattern;
  if (ns.args.length == 0){
    pattern = "";
    ns.tprint("Searching");
  } else {
    pattern = ns.args[0];
    ns.tprint("Searching with pattern " + pattern);
  }
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var servers = await crawler.crawl();
  await forEachAsync(servers, async function(i, e){
    await listFiles(ns, e, pattern);
  });
}