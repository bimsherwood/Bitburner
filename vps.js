/** @param {NS} ns **/

export function getVpsNames(){
  return [
    "vps-absol",
    "vps-arbok",
    "vps-arcanine",
    "vps-bonsly",
    "vps-buneary",
    "vps-charmander",
    "vps-clefairy",
    "vps-cubone",
    "vps-dartrix",
    "vps-diglet",
    "vps-dragonite",
    "vps-eevee",
    "vps-ekans",
    "vps-gengar",
    "vps-latios",
    "vps-marowak",
    "vps-meowth",
    "vps-nuzleaf",
    "vps-pikachu",
    "vps-poochyena",
    "vps-rattata",
    "vps-scorbunny",
    "vps-treecko",
    "vps-vulpix",
    "vps-zigzagoon"
  ];
}

async function quote(ns, size){
  var dollars = await ns.getPurchasedServerCost(size);
  var kilodollars = dollars / 1000;
  ns.tprint("$" + kilodollars + "k");
}

async function buy(ns, name, size){
  var newServer = await ns.purchaseServer(name, size);
  if (newServer == ""){
    ns.tprint("Failed to purchase server.");
  }
}

async function sell(ns, name){
  var success = await ns.deleteServer(name);
  if (!success){
    ns.tprint("Failed to sell server.");
  }
}

function Vps(ns, options){
  
  var hostname = options.hostname;
  var decommission = options.decommission;
  var commission = options.commission;
  var trace = options.trace;
  
  async function getSize(){
    var exists = await ns.serverExists(hostname);
    if (exists){
      return await ns.getServerMaxRam(hostname);
    } else {
      return 0;
    }
  }
  
  async function quote(size){
    return await ns.getPurchasedServerCost(size);
  }
  
  async function install(size){
    var exists = await ns.serverExists(hostname);
    if (exists){
      await decommission(hostname);
      var deleteSuccess = await ns.deleteServer(hostname);
      if(!deleteSuccess){
        await trace("Failed to delete server " + hostname);
        return false;
      }
    }
    var boughtServerName = await ns.purchaseServer(hostname, size);
    if(boughtServerName == ""){
      await trace("Failed to purchase server " + hostname);
      return false;
    }
    await ns.sleep(1000);
    await commission(hostname);
    return true;
  }
  
  async function considerUpgrade(levelIncrease){
    
    var currentSize = await getSize();
    var newSize;
    if (currentSize > 0){
      var newSize = currentSize * 2**levelIncrease;
    } else {
      var newSize = 2**(levelIncrease + 2); // At least 8
    }
    
    async function quoteNewSize(){
      return await quote(newSize);
    }
    
    async function installNewSize(){
      return await install(newSize);
    }
    
    return {
      hostname: hostname,
      size: newSize,
      quote: quoteNewSize,
      install: installNewSize
    };
    
  }

  async function currentState(){
    
    var currentSize = await getSize();
    
    async function quoteNothing(){
      return 0;
    }
    
    async function doNothing(){ }
    
    return {
      hostname: hostname,
      size: currentSize,
      quote: quoteNothing,
      install: doNothing
    };
    
  }
  
  return {
    hostname,
    currentState,
    considerUpgrade
  };
  
}

export function VpsManager(ns, options){
  
  var hostnames = options.hostnames;
  var decommission = options.decommission;
  var commission = options.commission;
  var trace = options.trace;
  
  var servers = hostnames.map(
    function(hostname){
      return new Vps(ns, {
        hostname,
        decommission,
        commission,
        trace
      });
    });
    
  async function quoteAll(proposals){
    var quote = 0;
    for(var i in proposals){
      quote += await proposals[i].upgrade.quote();
    }
    return quote;
  }
  
  async function currentPortfolio(){
    var serverStates = [];
    for(var i in servers){
      serverStates.push({
        server: servers[i],
        levelIncrease: 0,
        upgrade: await servers[i].currentState()
      });
    }
    return serverStates;
  }
  
  async function planUpgrade(){
    
    var proposals = await currentPortfolio();
    proposals.sort(function(a, b){ return a.upgrade.size - b.upgrade.size; });
    
    for(var i = 0; i < proposals.length; i++){
      
      // Current proposal
      var proposal = proposals[i];
      var server = proposal.server;
      var levelIncreaseBefore = proposal.levelIncrease;
      var upgradeBefore = proposal.upgrade;
      var quoteBefore = await upgradeBefore.quote();
      var totalQuoteBefore = await quoteAll(proposals);
      
      // Upgrade Server as much as possible
      for(var levelIncreaseAfter = levelIncreaseBefore; ; levelIncreaseAfter++){
        
        var upgradeAfter = await server.considerUpgrade(levelIncreaseAfter);
        var quoteAfter = await upgradeAfter.quote();
        var totalQuoteAfter = totalQuoteBefore - quoteBefore + quoteAfter;
        
        // Can afford it? Update the proposal
        var funds = await ns.getServerMoneyAvailable("home");
        if(totalQuoteAfter <= funds){
          proposal.levelIncrease = levelIncreaseAfter;
          proposal.upgrade = upgradeAfter;
        } else {
          break;
        }
        
      }
      
    }
    
    return proposals;
    
  }
  
  async function traceUpgrade(proposals){
    for(var i in proposals){
      if(proposals[i].levelIncrease > 0){
        await trace(
          proposals[i].server.hostname
          + ": +"
          + proposals[i].levelIncrease);
      }
    }
  }
  
  async function executeUpgrade(proposals){
    for(var i in proposals){
      await proposals[i].upgrade.install();
    }
  }
  
  async function upgrade(){
    var proposals = await planUpgrade();
    await traceUpgrade(proposals);
    await executeUpgrade(proposals);
  }
  
  return {
    upgrade
  }
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  vps.js quote <log2(ram)>");
  ns.tprint("  vps.js buy <name> <log2(ram)>");
  ns.tprint("  vps.js sell <name>");
  ns.tprint("  vps.js upgrade");
}

export async function main(ns) {
  
  var managerOptions = {
    hostnames: getVpsNames(),
    decommission: async function(hostname){
        await ns.killall(hostname);
      },
    commission: async function(hostname) {
        ns.tprint(hostname, " has been upgraded.");
      },
    trace: async function(msg){
        ns.tprint(msg);
      }
  };

  if (ns.args.length == 2 && ns.args[0] == "quote"){
    await quote(ns, 2**ns.args[1]);
  } else if (ns.args.length == 3 && ns.args[0] == "buy"){
    await buy(ns, ns.args[1], 2**ns.args[2]);
  } else if (ns.args.length == 2 && ns.args[0] == "sell"){
    await sell(ns, ns.args[1]);
  } else if (ns.args.length == 1 && ns.args[0] == "upgrade"){
    var manager = new VpsManager(ns, managerOptions);
    await manager.upgrade();
    ns.tprint("Done.");
  } else {
    printHelp(ns);
  }

}