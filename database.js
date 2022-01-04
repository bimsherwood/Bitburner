/** @param {NS} ns **/

import { Cache } from "cache.js";
import { DatabaseClient } from "database-client.js"
import {
    safeLoop,
    portSend,
    portTryReceive,
    portPeek,
    portClear
  } from "utils.js";

function DatabaseResponse(request, value){
  
  return {
    type: "response",
    channel: request.channel,
    database: request.database,
    key: request.key,
    value: value
  }
  
}

function DatabaseServer(ns){
  
  var responseTTL = 5000;
  var pollPeriod = 10;
  var lastResponseChannel = null;
  var lastResponseLife = 0;
  
  function clearResponseTimeout(){
    lastResponseChannel = null;
    lastResponseLife = 0;
  }
  
  async function serviceReadRequest(request){
    var cache = new Cache(ns, "db-" + request.database);
    var cacheCtxt = await cache.open();
    var value = cacheCtxt.load(request.key);
    var response = DatabaseResponse(request, value);
    await portSend(ns, "db", response);
  }
  
  async function serviceWriteRequest(request){
    var cache = new Cache(ns, "db-" + request.database);
    var cacheCtxt = await cache.open();
    cacheCtxt.save(request.key, request.value);
  }
  
  async function serviceResponse(response){
    if(response.channel == lastResponseChannel){
      lastResponseLife += pollPeriod;
    } else {
      lastResponseLife = 0;
    }
    lastResponseChannel = response.channel;
    if (lastResponseLife > responseTTL){
      await portTryReceive(ns, "db");
    }
  }
  
  async function run(){
    await portClear(ns, "db");
    for(;;){
      
      var message = await portPeek(ns, "db");
      if (message == "NULL PORT DATA") {
        await ns.sleep(10*pollPeriod);
      } else if (message.type == "read") {
        ns.print("Read request: ", message);
        await portTryReceive(ns, "db");
        await serviceReadRequest(message);
        await ns.sleep(pollPeriod);
      } else if (message.type == "write") {
        ns.print("Write request: ", message);
        await portTryReceive(ns, "db");
        await serviceWriteRequest(message);
        await ns.sleep(pollPeriod);
      } else if (message.type == "response"){
        ns.print("Unconsumed response: ", message);
        await serviceResponse(message);
        await ns.sleep(pollPeriod);
      } else {
        ns.print("Bad message: ", message);
        await portTryReceive(ns, "db");
      }
      
    }
  }
  
  return {
    run: run
  }
  
}

export async function main(ns){
  if(ns.args.length == 1 && ns.args[0] == "server"){
    var server = new DatabaseServer(ns);
    await server.run();
  } else if (ns.args.length == 3 && ns.args[0] == "read"){
    var database = ns.args[1];
    var key = ns.args[2];
    var client = new DatabaseClient(ns, database);
    var result = await client.read(key);
    ns.tprint(result);
  } else if (ns.args.length == 4 && ns.args[0] == "write"){
    var database = ns.args[1];
    var key = ns.args[2];
    var value = ns.args[3];
    var client = new DatabaseClient(ns, database);
    await client.write(key, value);
  } else {
    ns.tprint("Usage:");
    ns.tprint("  database.js server");
    ns.tprint("  database.js read <database> <key>");
    ns.tprint("  database.js write <database> <key> <value>");
  }
}