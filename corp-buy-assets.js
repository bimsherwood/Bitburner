/** @param {NS} ns **/

import { State, Commodity, stockStats } from "corp-common.js";

var assets = {};
assets["BimGronomy"] = {};
assets["BimGronomy"][Commodity.aiCores] = 1;
assets["BimGronomy"][Commodity.hardware] = 1;
assets["BimGronomy"][Commodity.realEstate] = 5;
assets["BimGronomy"][Commodity.robots] = 1;
assets["BimJections"] = {};
assets["BimJections"][Commodity.aiCores] = 3;
assets["BimJections"][Commodity.hardware] = 2;
assets["BimJections"][Commodity.realEstate] = 1;
assets["BimJections"][Commodity.robots] = 1;
assets["BimPutation"] = {};
assets["BimPutation"][Commodity.aiCores] = 2;
//assets["BimPutation"][Commodity.hardware] = 3;
assets["BimPutation"][Commodity.realEstate] = 2;
assets["BimPutation"][Commodity.robots] = 1;
assets["BimLectric"] = {};
assets["BimLectric"][Commodity.aiCores] = 4;
//assets["BimLectric"][Commodity.hardware] = 1;
assets["BimLectric"][Commodity.realEstate] = 8;
assets["BimLectric"][Commodity.robots] = 1;
assets["BimRock"] = {};
assets["BimRock"][Commodity.aiCores] = 1;
assets["BimRock"][Commodity.hardware] = 1;
assets["BimRock"][Commodity.realEstate] = 1;
assets["BimRock"][Commodity.robots] = 1;
assets["BimTopus"] = {};
assets["BimTopus"][Commodity.aiCores] = 1;
assets["BimTopus"][Commodity.hardware] = 1;
assets["BimTopus"][Commodity.realEstate] = 2;
assets["BimTopus"][Commodity.robots] = 1;

var warehouseBuffer = 0.1;

async function purchase(ns, division, city, commodity, amount){
  await ns.corporation.buyMaterial(
    division,
    city,
    commodity,
    Math.ceil(amount));
}

async function clearPurchase(ns, division, city, commodity){
  await ns.corporation.buyMaterial(division, city, commodity, 0);
}

export async function main(ns){
  
  for(;;){
  
    // Take measurements
    var maxCapacityStats = await stockStats(ns, State.sale);
    
    // For each division
    for(var divisionName in maxCapacityStats){
      if(divisionName in assets){
        
        var divisionAssets = assets[divisionName];
        var divisionStats = maxCapacityStats[divisionName];
        
        // For each city
        for(var cityName in divisionStats){
          var cityStats = divisionStats[cityName];
          
          // For each asset
          for(var assetName in divisionAssets){
            
            // Decide how much to buy
            var assetPurchaseRate = divisionAssets[assetName];
            var targetCapacity = cityStats.volume * (1 - warehouseBuffer);
            if(cityStats.stock < targetCapacity){
              var purchaseRate =
                (targetCapacity - cityStats.stock)
                * assetPurchaseRate
                / 100;
              await purchase(
                ns,
                divisionName,
                cityName,
                assetName,
                purchaseRate);
            } else {
              await clearPurchase(
                ns,
                divisionName,
                cityName,
                assetName);
            }
            
          }
          
        }
        
      }
      
    }
    
    await ns.sleep(5000);
  
  }
  
}