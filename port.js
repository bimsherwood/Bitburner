/** @param {NS} ns **/

import { getPortServer } from "./port-server.js";

function remove(array, elem){
  const index = array.indexOf(elem);
  if (index > -1) {
    array.splice(index, 1);
  }
}

export async function portSend(ns, channel, message){
  var portServer = getPortServer();
  var port = ns.getPortHandle(portServer.portNumber);
  await port.write({
    channel: channel,
    message: message
  });
}

export async function portSubscribe(ns, channel){
  
  var subscription = {
    channel: channel,
    messages: []
  };
  
  var portServer = getPortServer();
  portServer.subscriptions.push(subscription);
  ns.tprint("Added to subscriptions: " + portServer.subscriptions);
  
  function tryRead(){
    if (subscription.messages.length > 0){
      var message = subscription.messages.pop();
      return {
        success: true,
        message: message
      };
    } else {
      return {
        success: false,
        message: null
      };
    }
  }
  
  function unsubscribe(){
    remove(portServer.subscriptions, subscription);
  }
  
  return {
    tryRead,
    unsubscribe
  };
  
}