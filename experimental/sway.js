/** @param {NS} ns **/

import { forEach, forEachAsync, safeLoop } from "utils.js";

var swayModes;

function Sway(ns, options){
  
  var target = options.target;
  
  if(typeof(swayModes) == "undefined"){
    swayModes = { };
  }
  if(typeof(swayModes[target]) == "undefined") {
    swayModes[target] = "weaken";
  }
  
  function operation(){
    var mode = swayModes[target];
    switch(mode){
      case "Decrease":
        return async function () {
          await ns.weaken(target, { stock: true });
        };
      case "Increase":
        return async function () {
          await ns.weaken(target, { stock: true });
        };
    }
  }
  
  async function run(){
    await safeLoop(){
    }
  }
  
  return {
    run
  };   
  
}

async function main(ns){
  
}
