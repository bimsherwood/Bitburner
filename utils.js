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

export function GreedyAllocation(
    containers,
    getContainerCapacity,
    resources,
    getResourceSize){

  function copy(array){
    return [...array];
  }
  
  function removeBiggest(array, getSize, maxSize){
    
    // Scan the array to find the biggest element not greater than maxSize
    var matchSize = null;
    var matchIndex = null;
    for(var i = 0; i < array.length; i++){
      var iSize = getSize(array[i]);
      var isBetterCandidate =
        iSize <= maxSize &&
        (matchSize == null || iSize > matchSize);
      if(isBetterCandidate){
        matchSize = iSize;
        matchIndex = i;
      }
    }
    
    // If an element was found, return it and the remaining elements.
    if(matchIndex != null){
      var bigger = array.slice(0, matchIndex);
      var smaller = array.slice(matchIndex + 1, array.length);
      return array.splice(matchIndex,1)[0];
    } else {
      return null;
    }
    
  }

  function removeSmallest(array, getSize, minSize){
    return removeBiggest(array, function(x){ return -getSize(x); }, -minSize);
  }

  function allocate(){
    var remainingContainers = copy(containers);
    var remainingResources = copy(resources);
    var allocations = [];
    for(;;){
      
      // Grab the next smallest container
      var smallestContainer = removeSmallest(
          remainingContainers,
          getContainerCapacity,
          0);
      if (smallestContainer == null) break;
      
      // Create an allocation for the container
      var allocation = [smallestContainer, []];
      allocations.push(allocation);
      var containerCapacityRemaining = getContainerCapacity(smallestContainer);
      for(;;){
        
        // Grab the biggest resource that fits in the container
        var biggestResourceThatFits = removeBiggest(
            remainingResources,
            getResourceSize,
            containerCapacityRemaining);
        if (biggestResourceThatFits == null) break;
        
        // Allocate the resource to the container
        containerCapacityRemaining -= getResourceSize(biggestResourceThatFits);
        allocation[1].push(biggestResourceThatFits);
        
      }
      
    }
    return {
      allocations,
      unallocatedResources: remainingResources
    }
  }

  return {
    allocate
  }
  
}