/** @param {NS} ns **/

export async function main(ns){
  var crime = ns.args[0];
  for(;;){
    for(var i = 0; i < 20; i++){
      var wait = await ns.singularity.commitCrime(crime);
      await ns.sleep(wait);
      while(await ns.singularity.isBusy()){
        await ns.sleep(500);
      }
    }
    await ns.sleep(10000);
  }
}