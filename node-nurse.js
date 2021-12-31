/** @param {NS} ns **/

function defaultOptions(){
  return {
    maxNodes: 32,
    maxNodeLevel: 120,
    maxNodeRam: 64,
    maxNodeCores: 16
  };
};

export function NodeNurse(ns, options){
  
  var maxNodes = options.maxNodes;
  var maxNodeLevel = options.maxNodeLevel;
  var maxNodeRam = options.maxNodeRam;
  var maxNodeCores = options.maxNodeCores;
  
  async function buyNodes(){
    while(await ns.hacknet.numNodes() < maxNodes){
      var currentFunds = await ns.getServerMoneyAvailable("home");
      var cost = await ns.hacknet.getPurchaseNodeCost();
      if (cost > currentFunds) break;
      var newNode = await ns.hacknet.purchaseNode();
      if (newNode < 0) {
        ns.print("Attempted node purchase failed");
        break;
      }
      await ns.sleep(100);
    }
  }
  
  async function increaseOn(nodeId, max, getSize, getCost, upgrade){
    while(await getSize(nodeId) < max){
      var currentFunds = await ns.getServerMoneyAvailable("home");
      var cost = await getCost(nodeId, 1);
      if (cost > currentFunds) break;
      var success = await upgrade(nodeId);
      if (!success) {
        ns.print("Attempted upgrade failed");
        break
      };
      await ns.sleep(100);
    }
  }
  
  async function increase(max, getSize, getCost, upgrade){
    var nodeCount = await ns.hacknet.numNodes();
    for(var i = 0; i < nodeCount; i++){
      await increaseOn(i, max, getSize, getCost, upgrade);
      await ns.sleep(100);
    }
  }
  
  async function getCoreCount(i){
    var stats = await ns.hacknet.getNodeStats(i);
    return stats.cores;
  }
  
  async function getRamSize(i){
    var stats = await ns.hacknet.getNodeStats(i);
    return stats.ram;
  }
  
  async function getLevel(i){
    var stats = await ns.hacknet.getNodeStats(i);
    return stats.level;
  }
  
  async function maintain(){
    await buyNodes();
    await increase(
      maxNodeCores,
      getCoreCount,
      ns.hacknet.getCoreUpgradeCost,
      ns.hacknet.upgradeCore);
    await increase(
      maxNodeRam,
      getRamSize,
      ns.hacknet.getRamUpgradeCost,
      ns.hacknet.upgradeRam);
    await increase(
      maxNodeLevel,
      getLevel,
      ns.hacknet.getLevelUpgradeCost,
      ns.hacknet.upgradeLevel);
  }
  
  return {
    maintain
  };
  
}

export async function main(ns){
  var nurse = new NodeNurse(ns, defaultOptions());
  for(;;){
    await nurse.maintain();
    await ns.sleep(60 * 1000);
  }
}