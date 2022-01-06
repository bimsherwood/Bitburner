/** @param {NS} ns **/

import { safeLoop } from "./utils.js";

var _cellStates;
var getLocalCellState = function(instanceId){
  if(typeof(_cellStates) === "undefined"){
    _cellStates = [];
  }
  if(typeof(_cellStates[instanceId]) === "undefined"){
    _cellStates[instanceId] = {
      command: "idle",
      target: null
    };
  }
  return _cellStates[instanceId];
}

function Cell (ns, options) {
  
  var instanceId = options.instanceId;
  var cellState = getLocalCellState(instanceId);
  
  async function execute(){
    await safeLoop(ns, async function(){
      switch(cellState.command){
        case "hack":
          await ns.hack(cellState.target);
          break;
        case "weaken":
          await ns.weaken(cellState.target);
          break;
        case "grow":
          await ns.grow(cellState.target);
          break;
        default:
          await ns.sleep(6*1000);
          break;
      }
    });
  }
  
  return {
	  execute
  }
  
};

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  cell.js <instance number>");
  ns.tprint("  cell.js <instance number> hack <target>");
  ns.tprint("  cell.js <instance number> weaken <target>");
  ns.tprint("  cell.js <instance number> grow <target>");
}

export async function main(ns) {
  
  if(ns.args.length == 1){
    var instanceId = ns.args[0];
    var newCell = new Cell(ns, { instanceId });
    await newCell.execute();
  } else if(ns.args.length == 2 && ns.args[1] == "status"){
    var instanceId = ns.args[0];
    var cellState = getLocalCellState(instanceId);
    ns.tprint(
      "Cell ",
      instanceId,
      ": ",
      cellState.command,
      " ",
      cellState.target);
  } else if(ns.args.length == 3){
    var instanceId = ns.args[0];
    var command = ns.args[1];
    var target = ns.args[2];
    var cellState = getLocalCellState(instanceId);
    cellState.command = command;
    cellState.target = target;
  } else {
    printHelp(ns);
  }
  
}