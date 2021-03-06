/** @param {NS} ns **/

var portNames = {
  "telemetry" : 1,
  "db" : 2,
  "test" : 20
}

export async function safeLoop(ns, f){
	for(;;){
		await f();
		await ns.sleep(100);
	}
}

export function forEach(arr, f){
  for(var i = 0; i < arr.length; i++){
    f(i, arr[i]);
  }
}

export async function forEachAsync(arr, f){
  for(var i = 0; i < arr.length; i++){
    await f(i, arr[i]);
  }
}

export function pushAll(destination, newElements){
  forEach(newElements, function(i, e){
    destination.push(e);
  });
}

export async function portSend(ns, portName, message){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  await port.write(message);
}

export async function portReceive(ns, portName){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  while(await port.empty()){
    await ns.sleep(100);
  }
  return await port.read();
}

export async function portTryReceive(ns, portName){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  return await port.read();
}

export async function portPeek(ns, portName){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  return await port.peek();
}

export async function portClear(ns, portName){
  var portNumber = portNames[portName];
  var port = await ns.getPortHandle(portNumber);
  return await port.clear();
}
