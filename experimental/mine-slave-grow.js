/** @param {NS} ns */
export async function main(ns) {
	var wait = Math.floor(Math.random() * 1000 * 60);
	await ns.sleep(wait);
	for(;;){
		await ns.grow(ns.args[0]);
		await ns.sleep(100);
	}
}