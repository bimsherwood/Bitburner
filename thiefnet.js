/** @param {NS} ns **/

import { forEachAsync } from "./utils.js";
import { InstallThief } from "./install-thief.js";
import { thiefnetSettings } from "./thiefnet-settings.js";

function Thiefnet (ns, network){
  
  var installer = new InstallThief(ns);
  
  async function update(){
    await forEachAsync(network, async function (i, e){
      await installer.install(e.host, e.target, e.threads);
    });
  }
  
  async function uninstall(){
    await forEachAsync(network, async function (i, e){
      await installer.uninstall(e.host);
    });
  }
  
  return {
    update,
    uninstall
  };
  
}

function printHelp(ns){
  ns.print("Usage:");
  ns.print("  thiefnet.js update");
  ns.print("  thiefnet.js uninstall");
}

export async function main(ns) {
  var network = thiefnetSettings();
  var thiefnet = new Thiefnet(ns, network);
  var argc = ns.args.length;
  if(argc != 1){
    printHelp(ns);
  } else if (ns.args[0] == "update"){
    await thiefnet.update();
  } else if (ns.args[0] == "uninstall"){
    await thiefnet.uninstall();
  } else {
    printHelp(ns);
  }
}
