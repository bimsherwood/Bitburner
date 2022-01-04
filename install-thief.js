/** @param {NS} ns **/

export function InstallThief (ns){
  
  var scriptName = "thief.js";
  var utilsName = "utils.js";
  
  async function sendScript(host){
    await ns.scp(scriptName, host);
    await ns.scp(utilsName, host);
  }
  
  async function removeScript(host){
    await ns.rm(scriptName, host);
    await ns.rm(utilsName, host);
  }
  
  async function stopScript(host){
    await ns.scriptKill(scriptName, host);
  }
  
  async function startScript(host, target, threadCount){
    await ns.exec(scriptName, host, threadCount, target);
  }
  
  async function install(host, target, threadCount) {
    await stopScript(host);
    await sendScript(host);
    await startScript(host, target, threadCount);
  }
  
  async function installMax(host, target) {
    var ramRequired = await ns.getScriptRam(scriptName);
    await stopScript(host);
    var ramMax = await ns.getServerMaxRam(host);
    var ramUsed = await ns.getServerUsedRam(host);
    var ramAvailable = ramMax - ramUsed;
    var threadCount = Math.floor(ramAvailable / ramRequired);
    await sendScript(host);
    if (threadCount > 0){
      await startScript(host, target, threadCount);
    }
  }

  async function uninstall(host) {
    await stopScript(host);
    await removeScript(host);
  }
  
  return {
    install,
    installMax,
    uninstall
  };
  
}

function printHelp(ns){
  ns.tprint("Usage:");
  ns.tprint("  install-thief.js install <host> <target> <threads>");
  ns.tprint("  install-thief.js install-max <host> <target>");
  ns.tprint("  install-thief.js uninstall <host>");
}

function readArgs(ns){
  
  var argc  = ns.args.length;
  if(argc <= 0){
    return null;
  }
  
  var command = ns.args[0];
  if (command == "install" && argc == 4){
    var host = ns.args[1];
    var target = ns.args[2];
    var threads = ns.args[3];
    return {
      mode: "install",
      host: host,
      target: target,
      threads: threads
    };
  } else if (command == "install-max" && argc == 3){
    var host = ns.args[1];
    var target = ns.args[2];
    return {
      mode: "install-max",
      host: host,
      target: target
    };
  } else if(command == "uninstall" && argc == 2) {
    var host = ns.args[1];
    return {
      mode: "uninstall",
      host: host
    };
  } else {
    return null;
  }
  
}

export async function main(ns) {
  var installThief = new InstallThief(ns);
  var options = readArgs(ns);
  if (options == null){
    printHelp(ns);
  } else if(options.mode == "install"){
    await installThief.install(options.host, options.target, options.threads);
  } else if(options.mode == "install-max"){
    await installThief.installMax(options.host, options.target);
  } else if(options.mode == "uninstall"){
    await installThief.uninstall(options.host);
  } else {
    printHelp(ns);
  }
}