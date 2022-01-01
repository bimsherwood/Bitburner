/** @param {NS} ns **/

function printHelp (ns){
  ns.tprint("Usage:");
  ns.tprint("  vps.js quote <log2(ram)>");
  ns.tprint("  vps.js buy <name> <log2(ram)>");
}

async function quote (ns, size){
  var dollars = await ns.getPurchasedServerCost(size);
  var kilodollars = dollars / 1000;
  ns.tprint("$" + kilodollars + "k");
}

async function buy (ns, name, size){
  var newServer = await ns.purchaseServer(name, size);
  if (newServer == ""){
    ns.tprint("Failed to purchase server.");
  }
}

export async function main(ns) {

  if (ns.args.length == 2 && ns.args[0] == "quote"){
    await quote(ns, 2**ns.args[1]);
  } else if (ns.args.length == 3 && ns.args[0] == "buy"){
    await buy(ns, ns.args[1], 2**ns.args[2]);
  } else {
    printHelp(ns);
  }

}