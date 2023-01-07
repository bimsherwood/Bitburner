/** @param {NS} ns */

import { Finder } from "/find.js";
import { Recruiter } from "/miner/recruit.js";

function MineMaster(ns, host, target){

    var homeRamReserve = 100;
    var scriptRamRequirement = 1.8; // 1.7
    var workerProcessLimit = 25;

    var weakenScriptName = "/miner/mine-slave-weak.js";
    var growScriptName = "/miner/mine-slave-grow.js";
    var hackScriptName = "/miner/mine-slave-hack.js";

    var index = 0;
    function nextIndex(){
        index += 1;
        return index;
    }

    async function getHostInfo(){
        
        var hostMaxRam = await ns.getServerMaxRam(host);
        var hostUsedRam = await ns.getServerUsedRam(host);
        var reserve = host == "home" ? homeRamReserve : 0;
        var ramBudget = hostMaxRam - hostUsedRam - reserve;
        var threadBudget = Math.floor(ramBudget / scriptRamRequirement);

        if (threadBudget > workerProcessLimit){
            return {
                workerProcessCount: workerProcessLimit,
                workerProcessThreads: Math.floor(threadBudget / workerProcessLimit)
            };
        } else {
            return {
                workerProcessCount: threadBudget,
                workerProcessThreads: 1
            };
        }

    }

    async function enrollHost(){

        // Kill all
        await ns.scriptKill(weakenScriptName, host);
        await ns.scriptKill(growScriptName, host);
        await ns.scriptKill(hackScriptName, host);

        // Decide the size of the host
        var info = await getHostInfo();

        // Copy over files to the host
        await ns.scp([weakenScriptName, growScriptName, hackScriptName], host, "home");

        // Start half weaken, half grow scripts
        for(var i = 0; i < info.workerProcessCount; i++){
            var script = i < info.workerProcessCount / 2 ? weakenScriptName : growScriptName;
            await ns.exec(script, host, info.workerProcessThreads, target, nextIndex());
        }

    }

    async function getCurrentWork(){
        
        var scripts = await ns.ps(host);
        var weakenScripts = scripts.filter(function(o){ return o.filename == weakenScriptName; });
        var growScripts = scripts.filter(function(o){ return o.filename == growScriptName; });
        var hackScripts = scripts.filter(function(o){ return o.filename == hackScriptName; });

        return {
            weakenScripts,
            growScripts,
            hackScripts
        };

    }

    async function analysePerformance(){

		var targetSecurityLevel = await ns.getServerMinSecurityLevel(target) + 3;
		var targetFunds = await ns.getServerMaxMoney(target) * 2 / 3;
        var securityLevel = await ns.getServerSecurityLevel(target);
        var funds = await ns.getServerMoneyAvailable(target);

        return {
            highSecurity: securityLevel > targetSecurityLevel,
            highFunds: funds >= targetFunds
        };

    }

    async function increaseWeaken(currentWork){
        // Reduce Hack, or if there is no Hack, reduce Grow
        if(currentWork.hackScripts.length > 0){
            var hackScript = currentWork.hackScripts[0];
            await ns.kill(hackScript.filename, host, ...hackScript.args);
            await ns.exec(weakenScriptName, host, hackScript.threads, target, nextIndex());
        } else if(currentWork.growScripts.length > 0){
            var growScript = currentWork.growScripts[0];
            await ns.kill(growScript.filename, host, ...growScript.args);
            await ns.exec(weakenScriptName, host, growScript.threads, target, nextIndex());
        }
    }

    async function increaseGrow(currentWork){
        // Reduce Hack, or if there is no Hack, reduce Weaken
        if(currentWork.hackScripts.length > 0){
            var hackScript = currentWork.hackScripts[0];
            await ns.kill(hackScript.filename, host, ...hackScript.args);
            await ns.exec(growScriptName, host, hackScript.threads, target, nextIndex());
        } else if(currentWork.weakenScripts.length > 0){
            var weakenScript = currentWork.weakenScripts[0];
            await ns.kill(weakenScript.filename, host, ...weakenScript.args);
            await ns.exec(growScriptName, host, weakenScript.threads, target, nextIndex());
        }
    }

    async function increaseHack(currentWork){
        // Reduce Grow, or if there is no Grow, reduce Weaken
        if(currentWork.growScripts.length > 0){
            var growScript = currentWork.growScripts[0];
            await ns.kill(growScript.filename, host, ...growScript.args);
            await ns.exec(hackScriptName, host, growScript.threads, target, nextIndex());
        } else if(currentWork.weakenScripts.length > 0){
            var weakenScript = currentWork.weakenScripts[0];
            await ns.kill(weakenScript.filename, host, ...weakenScript.args);
            await ns.exec(hackScriptName, host, weakenScript.threads, target, nextIndex());
        }
    }

    async function adjustHost(){
        var currentWork = await getCurrentWork();
        var performance = await analysePerformance();
        if(performance.highSecurity){
            await increaseWeaken(currentWork);
            //await ns.tprint("Increase Weaken");
        } else if (performance.highFunds){
            await increaseHack(currentWork);
            //await ns.tprint("Increase Hack");
        } else {
            await increaseGrow(currentWork);
            //await ns.tprint("Increase Grow");
        }
    }

    return {
        enrollHost,
        adjustHost
    };

}

export async function main(ns) {

    // Get the target
    if (ns.args.length == 0){
        await ns.tprint("Usage: mine-master <target>");
        return;
    }
    var target = ns.args[0];

    // If this script is started in bootstrap mode, Kill
    // the manager if it is already running, then rerun
    // this script in active mode.
    if (ns.args.length == 1){
        var scriptName = await ns.getScriptName();
        var thisHost = await ns.getHostname();
        var instances = await ns.ps();
        var otherInstances = instances
            .filter(function(o){ return o.filename == scriptName; })
            .filter(function(o){ return o.pid != ns.pid; });
        for(var i in otherInstances){
            var otherInstance = otherInstances[i];
            await ns.tprint(otherInstance.pid);
            await ns.kill(otherInstance.filename, thisHost, ...otherInstance.args);
        }
        await ns.run(scriptName, 1, target, "activemode");
        return;
    }

    // Recruit all possible servers
    var finder = Finder(ns);
    var recruiter = Recruiter(ns);
    await finder.scan();
    var servers = finder.getServers();
    await recruiter.recruitAll(servers);

    // Install the miners
    var homeMaster = MineMaster(ns, "home", target);
    await homeMaster.enrollHost();
    var masters = [homeMaster];
    for (var i in servers){
        var host = servers[i];
        if(await ns.hasRootAccess(host)){
            var master = MineMaster(ns, host, target);
            await master.enrollHost();
            masters.push(master);
        }
    }

    // Perform periodic adjustments
    var reviewDelay = 1 * 60 * 1000; // 1 minute
    var stepDelay = 5 * 1000; // 5 seconds
    for(;;){
        for (var i in masters){
            var master = masters[i];
            await master.adjustHost();
            await ns.sleep(stepDelay);
        }
        await ns.sleep(reviewDelay);
    }

}