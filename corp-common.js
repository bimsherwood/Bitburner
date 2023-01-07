export var State = {
  start: "START",
  purchase: "PURCHASE",
  production: "PRODUCTION",
  sale: "SALE",
  export: "EXPORT"
};

export var Commodity = {
  aiCores: "AI Cores",
  energy: "Energy",
  food: "Food",
  hardware: "Hardware",
  plants: "Plants",
  realEstate: "Real Estate",
  robots: "Robots",
  water: "Water"
};

export async function awaitState(ns, state){
  for(;;){
    var corp = await ns.corporation.getCorporation();
    if(corp.state == state){
      return;
    }
    await ns.sleep(100);
  }
}

async function cityStockStats(ns, divisionName, cityName){
  
  var warehouse = ns.corporation.getWarehouse(divisionName, cityName);
  var volume = warehouse.size;
  var stock = warehouse.sizeUsed;
  
  var stats = {};
  stats.volume = volume;
  stats.stock = stock;
  
  return stats;
  
}

async function divisionStockStats(ns, divisionName){
  
  var division = ns.corporation.getDivision(divisionName);
  var cityNames = division.cities;
  
  var stats = {};
  for(var i in cityNames){
    var cityName = cityNames[i];
    if(ns.corporation.hasWarehouse(divisionName, cityName)){
      stats[cityName] = await cityStockStats(ns, divisionName, cityName);
    }
  }
  
  return stats;
  
}

export async function stockStats(ns, state){
  
  await awaitState(ns, state);
  
  var corporation = await ns.corporation.getCorporation();
  var divisions = corporation.divisions;
  var divisionNames = divisions.map(function(o){ return o.name; });
  
  var stats = {};
  for(var i in divisionNames){
    var divisionName = divisionNames[i];
    stats[divisionName] = await divisionStockStats(ns, divisionName);
  }
  
  return stats;
  
}

export async function allStockStats(ns){
  
  var stats = {};
  for(var i in State){
    var state = State[i];
    stats[state] = await stockStats(ns, state);
  }
  
  return stats;
  
}