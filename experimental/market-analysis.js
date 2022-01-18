/** @param {NS} ns **/

import { forEach, forEachAsync } from "./utils.js";

// An arithmetic mean, with a default of 0 for empty lists
function average(array){
  if(array.length > 0){
    return array.reduce(function (a,b) { return a + b; }) / array.length;
  } else {
    return 0;
  }
}

// A sliding-window collection of prices for a given stock.
export function StockHistory(options){
  
  var symbol = options.symbol;
  var windowSize = options.windowSize;
  
  // A window of prices from which statistics can be derived.
  var priceHistory = [];
  
  // Whether the window is primed (fully filled);
  function isWindowFull(){
    return priceHistory.length >= windowSize;
  }
  
  function logPrice(price){
    priceHistory.push(price);
    while(priceHistory.length > windowSize){
      priceHistory.shift();
    }
  }
  
  // The mean average of the step-by-step growth rate
  function averageGrowth(){
    
    if(priceHistory.length <= 1){
      return 0;
    }
    
    var growths = [];
    for(var i = 1; i < priceHistory.length; i++){
      var basePrice = priceHistory[i - 1];
      var nextPrice = priceHistory[i];
      if (basePrice != 0){
        var growth = (nextPrice - basePrice) / basePrice;
        growths.push(growth);
      }
    }
    
    return average(growths);
    
  }
  
  return {
    symbol,
    isWindowFull,
    logPrice,
    averageGrowth
  };
  
}

// Monitors the stock prices for the given stocks.
export function StockMonitor(ns, options){
  
  var stockSymbols = options.stockSymbols;
  var windowSize = options.windowSize;
  
  var stepTime = 6*1000;
  var stockHistories = stockSymbols.map(function(symbol){
    return new StockHistory({
      symbol: symbol,
      windowSize: windowSize
    });
  });
  
  async function logPrice(stockHistory){
    var askPrice = await ns.stock.getAskPrice(stockHistory.symbol);
    var bidPrice = await ns.stock.getBidPrice(stockHistory.symbol);
    var price = average([askPrice, bidPrice]);
    stockHistory.logPrice(price);
  }
  
  async function logPrices(){
    await forEachAsync(stockHistories, async function(i, e){
      await logPrice(e);
    });
  }
  
  // Takes a callback that is called whenever some stock histories are
  // ready for consumption. The callback is called with the empty array as long
  // as the histories are not primed with history yet.
  async function monitor(callback){
    for(;;){
      await logPrices();
      var primedHistories = stockHistories.filter(function(e){
        return e.isWindowFull();
      });
      await callback(primedHistories);
      await ns.sleep(stepTime);
    }
  }
  
  return {
    monitor
  };
  
}

export async function main(ns){
  
  var symbols = await ns.stock.getSymbols();
  var stockMonitor = new StockMonitor(ns, {
    stockSymbols: symbols,
    windowSize: 16
  });
  await stockMonitor.monitor(async function(stockHistories){
    
    var bestPerformances = stockHistories
      .sort(function(a, b){
        return b.averageGrowth() - a.averageGrowth();
      })
      .slice(0, 10);
      
    if(bestPerformances.length > 0){
      ns.tprint("===>");
      forEach(bestPerformances, function(i, e){
        ns.tprint(e.symbol, ": ", ns.nFormat(e.averageGrowth(), "0.000e+0"));
      });
    } else {
      ns.tprint("(Analysing market)");
    }
    
  });
  
}
