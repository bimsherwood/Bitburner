/** @param {NS} ns **/

import { forEach, pushAll } from "./utils.js";

function HostCollection(hostnames){
  
  var items = [...hostnames];
  
  function addAll(newHostnames){
    pushAll(items, newHostnames);
  }
  
  function notContains(hostname){
    return items.indexOf(hostname) < 0;
  }
  
  return {
    addAll,
    notContains
  }
  
}

function ServerRoute (ns, knownHosts, parentRoute, hostname){
  
  function getRoute(){
    return [hostname].concat([...parentRoute]);
  }
  
  async function findChildren(){
    var neighbours = await ns.scan(hostname);
    var newHosts = neighbours.filter(knownHosts.notContains);
    knownHosts.addAll(newHosts);
    var children = newHosts.map(function(childHostname){
        return new ServerRoute(ns, knownHosts, getRoute(), childHostname);
      });
    return children;
  }
  
  return {
    hostname,
    getRoute,
    findChildren
  }
  
}

export function TraceRoute (ns, options){
  
  var resultLimit = options.resultLimit;
  var rootHost = options.rootHost;
  
  async function crawl(){
    
    var knownHostCollection = new HostCollection([rootHost]);
    var rootRoute = new ServerRoute(ns, knownHostCollection, [], rootHost);
    var unscannedHosts = [rootRoute];
    var scannedHosts = [];
    
    while(unscannedHosts.length > 0){
      
      var nextHost = unscannedHosts.pop();
      
      var resultLimitReached =
        unscannedHosts.length > resultLimit ||
        scannedHosts.length > resultLimit;
      if(resultLimitReached){
        ns.print("Trace route result limit reached.");
        break;
      }
      
      var children = await nextHost.findChildren();
      pushAll(unscannedHosts, children);
      scannedHosts.push(nextHost);
      
    }
    
    return scannedHosts;
    
  }
  
  return {
    crawl
  };
  
}

function printRoute(ns, route, connectMode){
  var hostnames = route.getRoute().reverse();
  var joiner = connectMode ? "; connect " : " -> ";
  ns.tprint(hostnames.join(joiner));
}

export async function main(ns){
  
  var traceRoute = new TraceRoute(ns, {
    resultLimit: 1000,
    rootHost: "home"
  });
  var allRoutes = await traceRoute.crawl();
  
  var connectMode;
  if(ns.args.length >= 2 && ns.args[1] == "--connect"){
    connectMode = true;
  } else {
    connectMode = false;
  }
  
  var matchingRoutes;
  if(ns.args.length == 0){
    matchingRoutes = allRoutes;
  } else {
    var targetHostname = ns.args[0];
    matchingRoutes = allRoutes
      .filter(function(route){
        return route.hostname == targetHostname;
      });
  }
  
  forEach(matchingRoutes, function(i, route){
    printRoute(ns, route, connectMode);
  });

}