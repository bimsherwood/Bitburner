/** @param {NS} ns **/

import { DatabaseClient } from "./database-client.js";
import { safeLoop } from "./utils.js";

function Cell (ns, options) {
  
  var database = options.database;
  var localInstanceName = options.localInstanceName;
  
  async function dbRead(subkey){
    var globalInstanceName = await ns.getHostname() + "|" + localInstanceName;
    return await database.read(globalInstanceName + "-" + subkey);
  }
  
  async function dbWrite(subkey, data){
    var globalInstanceName = await ns.getHostname() + "|" + localInstanceName;
    return await database.write(globalInstanceName + "-" + subkey, data);
  }
  
  async function executeInstruction(instruction){
    var operation = instruction.operation;
    var target = instruction.target;
    switch(operation){
      case "grow":
        await ns.grow(target);
        break;
      case "weaken":
        await ns.weaken(target);
        break;
      case "hack":
        await ns.hack(target);
        break;
      default:
        await ns.sleep(1000);
        ns.print("Unknown operation: ", operation);
        break;
    }
  }
  
  async function execute(){
    
    var commandStatus = await dbRead("status");
    var commandInstruction = await dbRead("instruction");
    if(commandStatus == "continuous" && commandInstruction != null){
      ns.print("Continuous: ", commandInstruction);
      await executeInstruction(commandInstruction);
    } else if(commandStatus == "oneshot" && commandInstruction != null) {
      ns.print("One Shot: ", commandInstruction);
      await executeInstruction(commandInstruction);
      await dbWrite("status", "complete");
    } else {
      await ns.sleep(6*1000); // Idle
    }
    
  }

  async function run() {
    await safeLoop(ns, async function(){
      await execute();
    });
  }

  return {
    run
  };

};

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  cell.js <database> <local instance name>");
}

export async function main(ns) {
  
  var argc = ns.args.length;
  if(argc != 2){
    printHelp(ns);
    return;
  }
  
  var databaseName = ns.args[0];
  var instanceName = ns.args[1];
  var database = new DatabaseClient(ns, databaseName);
  var cell = new Cell(ns, {
    database: database,
    localInstanceName: instanceName
  });
  await cell.run();
  
}
