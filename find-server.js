/** @param {NS} ns **/

import { forEach, forEachAsync } from "utils.js";
import { Crawler } from "crawler.js";

export function ServerFinder(ns, options){
  
  var hostnames = options.hostnames;
  var limit = options.limit;
  var onlyWithRootAccess = options.onlyWithRootAccess;
  var onlyWithMoney = options.onlyWithMoney;
  var onlyNotMine = options.onlyNotMine;
  var onlyNotHome = options.onlyNotHome;
  var onlyLowSecurity = options.onlyLowSecurity;
    
  async function profileServers(){
    var profiles = [];
    await forEachAsync(hostnames, async function(i, e){
      profiles.push({
        hostname: e,
        hasRootAccess: await ns.hasRootAccess(e),
        maxMoney: await ns.getServerMaxMoney(e),
        maxRam: await ns.getServerMaxRam(e),
        minSecurity: await ns.getServerMinSecurityLevel(e),
      });
    });
    return profiles;
  }
  
  function getHostname(profile){
    return profile.hostname;
  }
  
  async function generateFilter(){
    
    var filters = [];
    
    if(onlyWithMoney){
      filters.push(function(serverProfile){
        return serverProfile.maxMoney > 0;
      });
    }
    
    if(onlyWithRootAccess){
      filters.push(function(serverProfile){
        return serverProfile.hasRootAccess;
      });
    }
    
    if(onlyNotMine){
      var myServers = await ns.getPurchasedServers();
      myServers.push("home");
      filters.push(function(serverProfile){
        return myServers.indexOf(serverProfile.hostname) < 0;
      });
    }
    
    if(onlyNotHome){
      filters.push(function(serverProfile){
        return serverProfile.hostname != "home";
      });
    }
    
    if(onlyLowSecurity){
      var hackLevel = await ns.getHackingLevel();
      filters.push(function(serverProfile){
        var hackRequirement = (
          3 *
          serverProfile.minSecurity *
          serverProfile.minSecurity);
        return hackLevel >= hackRequirement ||
          serverProfile.minSecurity == 1;
      });
    }
    
    return function(serverProfile){
      var passing = true;
      forEach(filters, function(i, e){
        passing = passing && e(serverProfile);
      });
      return passing;
    };
    
  }
  
  function scoreTarget(profile){  
    var moneyScore = Math.log10(profile.maxMoney);
    var securityScore = 100 - profile.minSecurity;
    // Rank by min security, but bonus points for more money
    return securityScore + moneyScore;
  }
  
  function scoreHost(profile){
    return profile.maxRam;
  }
  
  function compareTargetsDesc(a, b){
    return scoreTarget(b) - scoreTarget(a);
  }
  
  function compareHostsDesc(a, b){
    return scoreHost(b) - scoreHost(a);
  }
  
  async function findBest(compareDesc){
    var profiles = await profileServers();
    var filter = await generateFilter();
    return profiles
      .filter(filter)
      .sort(compareDesc)
      .slice(0, limit || Infinity)
      .map(getHostname);
  }
  
  async function findBestTargets(){
    return await findBest(compareTargetsDesc);
  }
  
  async function findBestHosts(){
    return await findBest(compareHostsDesc);
  }
  
  return {
    findBestTargets,
    findBestHosts
  };
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  find-server.js host <limit>");
  ns.tprint("  find-server.js target <limit>");
}

export async function main(ns){
  
  var findHosts = ns.args.length == 2 && ns.args[0] == "host";
  var findTargets = ns.args.length == 2 && ns.args[0] == "target";
  var showHelp = !findHosts && !findTargets;
  
  if(showHelp){
    printHelp(ns);
    return;
  }
  
  var crawler = new Crawler(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var servers = await crawler.crawl();
  
  var serverFinder = new ServerFinder(ns, {
    hostnames: servers,
    limit: ns.args[1],
    onlyWithMoney: findTargets,
    onlyWithRootAccess: true,
    onlyNotMine: findTargets,
    onlyNotHome: true
  });
  
  var results;
  if (findHosts){
    results = await serverFinder.findBestHosts();
  } else {
    results = await serverFinder.findBestTargets();
  }
  
  forEach(results, function(i, e){
    ns.tprint(i+1, ": ", e);
  });
  
}