/** @param {NS} ns **/

/**
 * Multiplexes the port system. Uses port 1.
 */

import { forEachAsync, safeLoop } from "./utils.js";

var portServer = {
  portNumber: 1,
  subscriptions: []
}

export function getPortServer(){
  return portServer;
}

export async function main(ns) {
  var port = ns.getPortHandle(portServer.portNumber);
  await safeLoop(ns, async function(){
    if(!port.empty()) {
      var packet = await port.read();
      ns.tprint("Servicing subscriptions: " + portServer);
      await forEachAsync(portServer.subscriptions, async function(i, e){
        if(e.channel == packet.channel){
          ns.tprint("Sent!");
          e.messages.push(packet.message);
        } else {
          ns.tprint("Skipped.");
        }
      });
    }
  });
}