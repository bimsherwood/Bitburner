/** @param {NS} ns **/

import { forEach } from "./utils.js";

export function Crawler(ns, options){
  
  var resultLimit = options.resultLimit;
  var rootHost = options.rootHost;
  
  // All the servers found
  var allDiscoveredServers = [];
  
  // These host names have already been scanned.
  var completeHosts = [];
  
  // These host names are recently discovered and
  // may not have been scanned.
  var discoveredHosts = [];

  async function analyzeServer(hostname){
    return {
      hostname,
      hasRootAccess: await ns.hasRootAccess(hostname)
    };
  }
  
  // Returns false when there is no more work to do.
  async function step(){
    
    // Run out
    if(discoveredHosts.length == 0){
      return false;
    }
    
    // Result limit reached
    if(discoveredHosts.length >= resultLimit){
      return false;
    }
    
    // Processing
    var hostname = discoveredHosts.pop();
    var alreadyScanned = completeHosts.indexOf(hostname) >= 0;
    if (!alreadyScanned){
      var server = await analyzeServer(hostname);
      allDiscoveredServers.push(server);
      completeHosts.push(hostname);
      var siblings = await ns.scan(hostname);
      forEach(siblings, function(i, e){
        discoveredHosts.push(e);
      });
    }
    return true;
    
  }
  
  async function crawl(){
    discoveredHosts.push(rootHost);
    while(await step()){
      await ns.sleep(10);
    }
    return allDiscoveredServers;
  }
  
  return {
    crawl
  };
  
}