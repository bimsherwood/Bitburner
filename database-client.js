/** @param {NS} ns **/

import { portSend, portTryReceive, portPeek } from "./utils.js";

async function DatabaseReadRequest(ns, database, key){
  
  var channel =
    await ns.getHostname() +
    "|" + await ns.getScriptName() +
    "|" + database
    "|" + key;
  
  return {
    type: "read",
    channel: channel,
    database: database,
    key: key
  }
  
}

async function DatabaseWriteRequest(ns, database, key, value){
  
  var channel =
    await ns.getHostname() +
    "|" + await ns.getScriptName() +
    "|" + database
    "|" + key;
  
  return {
    type: "write",
    channel: channel,
    database: database,
    key: key,
    value: value
  }
  
}

export function DatabaseClient(ns, database){
  
  async function sendReadRequest(key){
    var request = await DatabaseReadRequest(ns, database, key);
    await portSend(ns, "db", request);
    return request;
  }
  
  async function sendWriteRequest(key, value){
    var request = await DatabaseWriteRequest(ns, database, key, value);
    await portSend(ns, "db", request);
    return request;
  }
  
  async function awaitReadResponse(request){
    var timeout = 5000;
    var pollPeriod = 100;
    for(var time = 0; time < timeout; time += pollPeriod){
      await ns.sleep(pollPeriod);
      var message = await portPeek(ns, "db");
      var myResponse =
        message != null &&
        message.type == "response" &&
        message.channel == request.channel;
      if (myResponse){
        await portTryReceive(ns, "db");
        return message.value;
      }
    }
  }
  
  async function read(key){
    var request = await sendReadRequest(key);
    return await awaitReadResponse(request);
  }
  
  async function write(key, value){
    var request = await sendWriteRequest(key, value);
  }
  
  return {
    read: read,
    write: write
  }
  
}