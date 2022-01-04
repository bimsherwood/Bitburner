/** @param {NS} ns **/

import { DatabaseClient } from "database-client.js";

export async function main(ns){
  
  if(ns.args.length != 2){
    ns.tprint("Usage:");
    ns.tprint("  agent.js <database> <key containing function>");
    return;
  }
  
  var databaseName = ns.args[0];
  var key = ns.args[1];
  var database = new DatabaseClient(ns, databaseName);
  var job = await database.read(key);
  if(job != null){
    ns.tprint("Running job ", databaseName, ".", key);
    ns.tprint(job.run);
    await job.run();
  } else {
    ns.tprint("Failed to find job ", databaseName, ".", key);
  }
  
}
