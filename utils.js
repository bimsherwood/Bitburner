/** @param {NS} ns **/

export async function safeLoop(ns, f){
	for(;;){
		await f();
		await ns.sleep(100);
	}
}

export async function forEach(arr, f){
  for(var i = 0; i < arr.length; i++){
    f(i, arr[i]);
  }
}

export async function forEachAsync(arr, f){
  for(var i = 0; i < arr.length; i++){
    await f(i, arr[i]);
  }
}