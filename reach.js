/** @param {NS} ns **/

import { Cache } from "./cache.js";
import { Crawler } from "./crawler.js";
import { InstallThief } from "./install-thief.js";
import { portPoppers, rootServer } from "./root-server.js";
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
  
  async function isVulnerable(server){
    var myLevel = await ns.getHackingLevel();
    return server.requiredHackingLevel <= myLevel
      && server.numPortsRequired <= portPoppers(ns).length;
  }
  
  function alreadyInstalledOn(server) {
    return getInfectedServers().indexOf(server.hostname) >= 0;
  }
  
  async function considerServer(server){
    
    // Root the server if we can
    var shouldRootServer = !server.hasRootAccess
      && await isVulnerable(server);
    if (shouldRootServer){
      ns.tprint("Rooting server " + server.hostname);
      await rootServer(ns, server.hostname);
      server.hasRootAccess = true;
    }
    
    // Install if we can
    var shouldIntall =
      server.hasRootAccess &&
      !alreadyInstalledOn(server) &&
      server.hostname != "home";
    if (shouldIntall){
      ns.tprint("Installing on server " + server.hostname);
      await install(server, getTarget());
      getInfectedServers().push(server.hostname);
    }
    
  }
  
  async function discover(){
    await init();
    var servers = await crawler.crawl();
    await forEachAsync(servers, async function(i, e){
      await considerServer(e);
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
  ns.tprint("Usage:");
  ns.tprint("  reach.js discover");
  ns.tprint("  reach.js update");
  ns.tprint("  reach.js new-target <target>");
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
    ns.tprint("Done.");
  } else if (ns.args.length == 1 && ns.args[0] == "update"){
    await reach.update();
    ns.tprint("Done.");
  } else if (ns.args.length == 2 && ns.args[0] == "new-target"){
    await reach.changeTarget(ns.args[1]);
    ns.tprint("Done.");
  } else {
    printHelp(ns);
  }
  
}