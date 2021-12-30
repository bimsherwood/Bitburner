/** @param {NS} ns **/

import { Cache } from "./cache.js";
import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { forEachAsync } from "./utils.js";

function Reach(ns, cache, crawler, install){
  
  var getTarget;
  var setTarget;
  var getInfectedServers;
  
  async function init(){
    
    var cacheContext = await cache.open();
    
    getTarget = function(){
      var existingTarget = cacheContext.load("target");
      if (existingTarget){
        return existingTarget;
      } else {
        return "n00dles"; // Default
      }
    }
    
    setTarget = function (newTarget){
      cacheContext.save("target", newTarget);
    }
    
    getInfectedServers = function (){
      var hostList = cacheContext.load("infected-servers");
      if(hostList){
        return hostList;
      } else {
        var emptyList = [];
        cacheContext.save("infected-servers", emptyList);
        return emptyList;
      }
    }
    
  }
  
  function isVulnerable(server){
    return server.hasRootAccess;
  }
  
  function alreadyInstalledOn(server) {
    return getInfectedServers().indexOf(server.hostname) >= 0;
  }
  
  async function discover(){
    await init();
    var servers = await crawler.crawl();
    await forEachAsync(servers, async function(i, e){
      if (!alreadyInstalledOn(e) && isVulnerable(e)){
        await install(e, getTarget());
        getInfectedServers().push(e.hostname);
      }
    });
  }
  
  async function update(){
    await init();
    getInfectedServers().length = 0;
    await discover();
  }
  
  async function changeTarget(newTarget){
    await init();
    setTarget(newTarget);
    await update();
  }
  
  return {
    discover,
    update,
    changeTarget
  };
  
}

function printHelp(ns){
  ns.print("Usage:");
  ns.print("  reach.js discover");
  ns.print("  reach.js update");
  ns.print("  reach.js change-target <target>");
}

export async function main(ns) {
  
  var installer = new InstallThief(ns);
  var cache = new Cache(ns, "reach");
  var crawler = new Crawler(ns, {
    resultLimit: 100,
    rootHost: "home"
  });

  async function install(server, target){
    await installer.installMax(server.hostname, target);
  }
  
  var reach = new Reach(ns, cache, crawler, install);

  if (ns.args.length == 1 && ns.args[0] == "discover"){
    await reach.discover();
  } else if (ns.args.length == 1 && ns.args[0] == "update"){
    await reach.update();
  } else if (ns.args.length == 2 && ns.args[0] == "change-target"){
    await reach.changeTarget(ns.args[1]);
  } else {
    printHelp(ns);
  }
  
}