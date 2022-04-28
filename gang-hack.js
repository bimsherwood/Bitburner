/** @param {NS} ns **/

import { forEachAsync, safeLoop } from "utils.js";

export function getMemberNames(){
  return [
    "gang-absol",
    "gang-arbok",
    "gang-arcanine",
    "gang-bonsly",
    "gang-buneary",
    "gang-charmander",
    "gang-clefairy",
    "gang-cubone",
    "gang-dartrix",
    "gang-diglet",
    "gang-dragonite",
    "gang-eevee",
    "gang-ekans",
    "gang-gengar",
    "gang-marowak",
    "gang-meowth",
    "gang-pikachu",
    "gang-rattata",
    "gang-scorbunny",
    "gang-vulpix"
  ];
}

function Hiring(ns){
  
  async function update(){
    
    // Can recruit?
    var canRecruit = await ns.gang.canRecruitMember();
    if(!canRecruit) return;
    
    // Find an available name
    var suggestedMemberNames = getMemberNames();
    var existingMembers = await ns.gang.getMemberNames();
    var nextMemberName = null;
    for (var i in suggestedMemberNames) {
      var suggestedMemberName = suggestedMemberNames[i];
      if(existingMembers.indexOf(suggestedMemberName) < 0){
        nextMemberName = suggestedMemberName;
        break;
      }
    }
    
    // Recruit
    await ns.gang.recruitMember(nextMemberName);
    
  }
  
  return {
      update
  };
  
}

function Ascension(ns){
  
  var targetAscensionMultiplier = Math.sqrt(2);
  
  function getMultipliers(memberInfo, ascensionResults){
    var multipliers = [];
    multipliers.push({
      name: "HACK",
      ascendMultiplier: ascensionResults.hack
    });
    /*
    multipliers.push({
      name: "AGI",
      ascendMultiplier: ascensionResults.agi
    });
    multipliers.push({
      name: "CHA",
      ascendMultiplier: ascensionResults.cha
    });
    multipliers.push({
      name: "DEF",
      ascendMultiplier: ascensionResults.def
    });
    multipliers.push({
      name: "DEX",
      ascendMultiplier: ascensionResults.dex
    });
    multipliers.push({
      name: "STR",
      ascendMultiplier: ascensionResults.str
    });
    */
    return multipliers;
  }
  
  async function update(){
    var members = ns.gang.getMemberNames();
    await forEachAsync(members, async function(i, member){
      var memberInfo = ns.gang.getMemberInformation(member);
      var ascensionResults = ns.gang.getAscensionResult(member);
      if(typeof(ascensionResults) != "undefined"){
        var multipliers = getMultipliers(memberInfo, ascensionResults);
        var ascensionReady = multipliers.some(function(multiplier){
          return multiplier.ascendMultiplier >= targetAscensionMultiplier;
        });
        if(ascensionReady){
          ns.print("Ascending ", member);
          await ns.gang.ascendMember(member);
        }
      }
    });
  }
  
  return {
      update
  };
  
}

function Equipment(ns){
  
  var equipments = [
    "NUKE Rootkit",
    "Soulstealer Rootkit",
    "Demon Rootkit",
    "Hmap Node",
    "Jack the Ripper"
  ];
  
  async function costThreshold(funds, cost, memberCount){
    // Have at least 10x the cash to buy the equipment for all members.
    var reserveMultiple = 10;
    return funds > cost * memberCount * reserveMultiple;
  }
  
  async function update(){
    var members = ns.gang.getMemberNames();
    var memberCount = members.length;
    await forEachAsync(members, async function(i, member){
      var memberInfo = await ns.gang.getMemberInformation(member);
      await forEachAsync(equipments, async function(j, equipment){
        var hasEquipment = memberInfo.upgrades.some(function(o){
          return o == equipment;
        });
        if(hasEquipment) return;
        var funds = await ns.getServerMaxMoney("home");
        var cost = await ns.gang.getEquipmentCost(equipment);
        var costEffective = costThreshold(funds, cost, memberCount);
        if(!costEffective) return;
        ns.print("Buying equipment ", equipment, " for ", member);
        await ns.gang.purchaseEquipment(member, equipment);
      });
    });
    
  }
  
  return {
      update
  };
  
}

function Assignment(ns){
  
  var moneyJobsByDifficulty = [
    "Ransomware",
    "Phishing",
    "Identity Theft",
    "Fraud & Counterfeiting",
    "Money Laundering"
  ];
  
  var respectJobsByDifficulty = [
    "DDOS",
    "Plant Virus",
    "Cyberterrorism"
  ];
  
  var reduceHeatJob = "Ethical Hacking";
  
  async function getMembersSkills(members){
    var membersAndSkills = [];
    await forEachAsync(members, async function(i, member){
      var memberInfo = await ns.gang.getMemberInformation(member);
      var hackSkill = memberInfo.hack;
      membersAndSkills.push({
        name: member,
        hackSkill: hackSkill
      });
    });
    return membersAndSkills;
  }
  
  async function wantedLevelHigh(){
    var gangInfo = await ns.gang.getGangInformation();
    var wantedLevelHigh = gangInfo.wantedLevel > 4;
    return wantedLevelHigh;
  }
  
  async function wantedLevelGrowing(){
    var gangInfo = await ns.gang.getGangInformation();
    var wantedLevelGrowing = gangInfo.wantedLevelGainRate > 0;
    return wantedLevelGrowing;
  }
  
  async function assignReduceHeatTask(member){
    await ns.gang.setMemberTask(member, reduceHeatJob);
  }
  
  async function optimiseMoneyTask(member){
    
    var easiestJob = moneyJobsByDifficulty[0];
    
    // Start the member at the easiest job
    await ns.gang.setMemberTask(member, easiestJob);
    var lastJob = easiestJob;
    var initialInfo = await ns.gang.getMemberInformation(member);
    var bestPerformance = initialInfo.moneyGain;
    
    // Promote. If performance declines, demote and stop
    for(var i = 1; i < moneyJobsByDifficulty.length; i++){
      var job = moneyJobsByDifficulty[i];
      
      // Experiment
      await ns.gang.setMemberTask(member, job);
      await ns.sleep(1200);
      
      // Check the results of the experiment
      var promotionInfo = await ns.gang.getMemberInformation(member);
      var promotionPerformance = promotionInfo.moneyGain;
      var wantedLevelBad =
        await wantedLevelHigh() ||
        await wantedLevelGrowing();
        
      // Move along, or Fuck Go Back
      var tryNextPromotion =
        promotionPerformance >= bestPerformance &&
        !wantedLevelBad;
      if(tryNextPromotion){
        lastJob = job;
      } else {
        await ns.gang.setMemberTask(member, lastJob);
        break;
      }
    }
    
  }
  
  async function update(){
    
    var members = ns.gang.getMemberNames();
    var membersAndSkills = await getMembersSkills(members);
    membersAndSkills.sort(function(a, b){
      return b.hackSkill - a.hackSkill; // Desc
    });
    
    // Best member will reduce wanted level
    if(membersAndSkills.length == 0) return;
    await ns.gang.setMemberTask(membersAndSkills[0].name, "Ethical Hacking");
    
    // Don't continue if wanted level is too high, just
    // wait for it to come down a bit before we experiment
    var tooHigh = await wantedLevelHigh();
    var growing = await wantedLevelGrowing();
    if(tooHigh && !growing) return;
    
    // Experiment with and assign money-making tasks
    var remainingMembers = membersAndSkills.slice(1);
    await forEachAsync(remainingMembers, async function(i, member){
      await ns.sleep(1200);
      await optimiseMoneyTask(member.name);
    });
    
  }
  
  return {
      update
  };
  
}

export async function main(ns){
  
  var routines = [
    Hiring(ns),
    Ascension(ns),
    Equipment(ns),
    Assignment(ns)
  ];
  
  await safeLoop(ns, async function(){
    await forEachAsync(routines, async function(i, routine){
      await routine.update();
    });
    await ns.sleep(60 * 1000);
  });
  
}
