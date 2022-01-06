/** @param {NS} ns **/

import { safeLoop, forEach, forEachAsync } from "./utils.js";

var transactionCost = 100*1000;
var packetSize = 1000*1000*1000;
var buyInGrowth = 0.55;
var sellGrowth = 0.5;
  
async function analyseMarket(ns){
  var symbols = await ns.stock.getSymbols();
  var profiles = [];
  await forEachAsync(symbols, async function(i, e){
    var position = await ns.stock.getPosition(e);
    var ownedShares = position[0];
    var maxShares = await ns.stock.getMaxShares(e);
    profiles.push({
      symbol: e,
      shares: ownedShares,
      askPrice: await ns.stock.getAskPrice(e),
      bidPrice: await ns.stock.getBidPrice(e),
      growthChance: await ns.stock.getForecast(e),
      maxShares: maxShares,
      availableShares: maxShares - ownedShares
    });
  });
  return profiles;
}

function bestForecast(profiles){
  return profiles
    .filter(function(profile){
      var maxPurchase = profile.askPrice * profile.availableShares;
      return maxPurchase > packetSize;
    })
    .sort(function(a,b){
      return b.growthChance - a.growthChance
    })
    [0];
}

async function sellRisky(ns, profiles){
  await forEachAsync(profiles, async function(i ,e){
    var sellValue = e.bidPrice * e.shares;
    var sell =
      sellValue > transactionCost &&
      e.growthChance <= sellGrowth;
    if(sell){
      ns.tprint(
        "Selling ",
        e.symbol,
        " ",
        ns.nFormat(e.shares * e.bidPrice, '($ 0.00 a)'));
      await ns.stock.sell(e.symbol, e.shares);
    }
  });
}

async function buyBest(ns, profiles){
  var funds = await ns.getServerMoneyAvailable("home");
  var bestStock = bestForecast(profiles);
  if(bestStock.growthChance > buyInGrowth){
    var maxPurchase = bestStock.askPrice * bestStock.availableShares;
    var maxPackets = Math.floor(maxPurchase / packetSize);
    var fundablePackets = Math.floor((funds - transactionCost) / packetSize);
    var packets = Math.min(maxPackets, fundablePackets);
    var shares = Math.floor(packets * packetSize / bestStock.askPrice);
    if(packets > 0){
      ns.tprint(
        "Buying ",
        bestStock.symbol,
        " ",
        ns.nFormat(shares * bestStock.askPrice, '($ 0.00 a)'));
      await ns.stock.buy(bestStock.symbol, shares);
    }
  }
}

async function printNetWorth(ns, analysis){
  var total = 0;
  forEach(analysis, function(i, e){
    if(e.shares > 0){
      total += e.shares * e.bidPrice;
    }
  });
  ns.tprint("Current value: ", ns.nFormat(total, '($ 0.00 a)'));
}

export async function main(ns){
  
  async function loop(f){
    for(;;){
      var analysis = await analyseMarket(ns);
      await f(ns, analysis);
      await ns.sleep(6*1000);
    }
  }
  
  var f;
  if(ns.args.length == 1 && ns.args[0] == "buy"){
    await loop(buyBest);
  } else if(ns.args.length == 1 && ns.args[0] == "sell"){
    await loop(sellRisky);
  } else if(ns.args.length == 1 && ns.args[0] == "show-value"){
    var analysis = await analyseMarket(ns);
    await printNetWorth(ns, analysis);
    return;
  } else {
    ns.tprint("Usage:");
    ns.tprint("  bubble-rider.js buy");
    ns.tprint("  bubble-rider.js sell");
    ns.tprint("  bubble-rider.js show-value");
    return;
  }
  
}
