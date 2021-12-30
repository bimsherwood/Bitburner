/** @param {NS} ns **/

import { openCache } from "./cache-server.js";

export function Cache(ns, name){
  
  var cacheLibScriptName = "cache-server.js";
  var localhost = ns.getHostname();
  var cache = null;
  
  function load(key){
    return cache[key];
  }

  function save(key, value){
    cache[key] = value;
  }
  
  async function open(){
  
    // Ensure cache server is loaded
    if(!await ns.scriptRunning(cacheLibScriptName, localhost)){
      await ns.exec(cacheLibScriptName, localhost);
    }
    
    cache = openCache(name);
    
    return {
      load,
      save
    };
    
  }
  
  return {
    open
  };
  
}

function printHelp(ns){
  ns.print("Usage:");
  ns.print("  cache.js <cache-name> <key>");
  ns.print("  cache.js <cache-name> <key> <value>");
}

export async function main(ns){
  if (ns.args.length == 2){
    var dbName = ns.args[0];
    var key = ns.args[1];
    var cache = await (new Cache(ns, dbName)).open();
    ns.print(cache.load(key));
  } else if (ns.args.length == 3){
    var dbName = ns.args[0];
    var key = ns.args[1];
    var value = ns.args[2];
    var cache = await (new Cache(ns, dbName)).open();
    cache.save(key, value);
  } else {
    printHelp(ns);
  }
}