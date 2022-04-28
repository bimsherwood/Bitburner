/** @param {NS} ns **/

export function Analysis(ns, options){
  
  var stockCode = options.stockCode;
  var historySize = options.historySize;
  
  var averagePriceHistory = [];
  
  function getAveragePrice(windowSize, lookBack){
    var startAt = averagePriceHistory.length - windowSize - lookBack;
    var recordings = averagePriceHistory.slice(startAt);
    var average = recordings.reduce((a,b) => (a+b)) / recordings.length;
    return average;
  }
  
  function getTrend(windowSize, lookBack){
    var startAt = averagePriceHistory.length - windowSize - lookBack;
    var endAt = lookBack;
    if(startAt >= 0 && endAt < averagePriceHistory.length){
      var start = averagePriceHistory[startAt];
      var end = averagePriceHistory[endAt];
      var rise = end - start;
      var growth = rise / start;
      return growth;
    } else {
      return null;
    }
  }
  
  async function takeRecording(){
    var currentPrice = await ns.stock.getPrice(stockCode);
    averagePriceHistory.push(currentPrice);
    if(averagePriceHistory.length > historySize){
      averagePriceHistory.shift();
    }
  }
  
  return {
    getAveragePrice,
    getTrend,
    takeRecording
  };
  
}

export function Market(ns, options){
  
  var stockCodes = options.stockCodes;
  var historySize = options.historySize;
  var trackers = {};
  
  async function takeRecording(){
    for(var i in trackers){
      var tracker = trackers[i];
      await tracker.takeRecording();
    }
  }
  
  function getTrendFor(symbol, windowSize, lookBack){
    var tracker = trackers[symbol];
    var trend = tracker.getTrend(windowSize, lookBack);
    return trend;
  }
  
  (function(){
    for(var i in stockCodes){
      var symbol = stockCodes[i];
      var analysis = Analysis(ns, {
        stockCode: symbol,
        historySize: historySize
      });
      trackers[symbol] = analysis;
    }
  })();
  
  return {
    takeRecording,
    getTrendFor
  };
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  run market-analysis.js <stock code>");
}

export async function main(ns){
  
  if(ns.args.length != 1){
    printHelp(ns);
    return;
  }
  
  var stockCode = ns.args[0];
  var analysis = Analysis(ns, {
    stockCode: stockCode,
    historySize: 3000
  });
  
  for(;;){
    await ns.sleep(1000);
    await analysis.takeRecording();
    var fastAverage = analysis.getAveragePrice(25, 0);
    var recentTrend = analysis.getTrend(25, 0);
    ns.print("----- ===== -----");
    ns.print(stockCode, ": ", fastAverage, ", ", recentTrend, "%");
  }
  
}
