/** @param {NS} ns */

import { numberWithMagnitude } from "format.js";

export function Hedge(ns){

	var minPurchaseSize = 10 * 1000 * 1000;
	var commission = 200 * 1000;
	var reserve = 1000 * 1000;
	var portfolioSize = 4;

	async function evaluate(stockCode){
		var askPrice = await ns.stock.getAskPrice(stockCode);
		var bidPrice = await ns.stock.getBidPrice(stockCode);
		var growth = await ns.stock.getForecast(stockCode) * 2 - 1;
		var longPosition = (await ns.stock.getPosition(stockCode))[0];
		var maxShares = await ns.stock.getMaxShares(stockCode);
		var sharesAvailable = maxShares - longPosition;
		return {
			stockCode,
			askPrice,
			bidPrice,
			growth,
			longPosition,
			sharesAvailable
		}
	}

	async function buyPortfolio(evaluations, targetPortfolioSize){

		// Decide how much to spend
		var fundsAvailable = await ns.getServerMoneyAvailable("home") - reserve;
		var fundsPerStock = fundsAvailable / targetPortfolioSize;
		var targetBuyOrderPerStock = fundsPerStock - commission;

		// Do nothing if funds are insufficient
		if(targetBuyOrderPerStock < minPurchaseSize){
			return;
		}
		
		// Filter evaluations
		var candidates = evaluations
			// Require positive growth
			.filter(function(o){
				return o.growth > 0;
			})
			// Require the max possible purchase to be at least the minimum purchase size
			.filter(function(o){
				var maxPossibleBuyOrder = (o.sharesAvailable * o.askPrice) + commission;
				return maxPossibleBuyOrder >= minPurchaseSize;
			});

		// Sort the candidates
		candidates.sort(function(a, b){ return b.growth - a.growth; });
		
		// Take the best ones
		var portfolio = candidates.slice(0, targetPortfolioSize);

		// Buy the stocks
		for(var i in portfolio){
			var pos = portfolio[i];

			// Decide how many shares to buy, or buy them all if
			// there are not enough.
			var maxPossibleBuyOrder = pos.sharesAvailable * pos.askPrice;
			var shares;
			if(targetBuyOrderPerStock < maxPossibleBuyOrder){
				shares = Math.floor(targetBuyOrderPerStock / pos.askPrice);
			} else {
				shares = pos.sharesAvailable;
			}

			// Buy the stock
			var purchasePrice = shares * pos.askPrice + commission;
			await ns.tprint("Buying $" + numberWithMagnitude(purchasePrice) + " of " + pos.stockCode);
			await ns.stock.buyStock(pos.stockCode, shares);
			
		}

	}

	async function sellBad(evaluations){
		for(var i in evaluations){
			var pos = evaluations[i];
			if(pos.growth < 0 && pos.longPosition > 0){
				var price = pos.longPosition * pos.bidPrice;
				await ns.tprint("Selling $" + numberWithMagnitude(price) + " of " + pos.stockCode);
				await ns.stock.sellStock(pos.stockCode, pos.longPosition);
			}
		}
	}

	async function hedge(){
		
		var stockCodes = await ns.stock.getSymbols();
		var evaluations = [];
		for(var i in stockCodes){
			evaluations.push(await evaluate(stockCodes[i]));
		}

		try {
			await sellBad(evaluations);
		} catch (ex) {
			await ns.print(ex);
		}

		try {
			await buyPortfolio(evaluations, portfolioSize);
		} catch (ex) {
			await ns.print(ex);
		}

	}

	return {
		hedge
	};

}

export async function main(ns) {
	var hedge = Hedge(ns);
	for(;;){
		await hedge.hedge();
		await ns.sleep(10*1000);
	}
}