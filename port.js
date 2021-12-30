/** @param {NS} ns **/

import { safeLoop } from "./utils.js";

var portNames = {
  "telemetry" : 1,
  "testing" : 20
}

export async function portSend(ns, portName, message){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  port.send(message);
}

export async function portReceive(ns, portName){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  while(port.empty()){
    await ns.sleep(100);
  }
  return port.read();
}

export async function portTryReceive(ns, portName){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  return port.read();
}