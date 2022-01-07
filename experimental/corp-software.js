/** @param {NS} ns **/

function randomSoftwareName(){
  
  var brands = [
    "TowerCrane",
    "Tellus",
    "Boldane",
    "Senkle",
    "Rorn",
    "QUAD",
    "Nastal"
  ];
  var applications = [
    "Backups",
    "Antivirus",
    "VPN",
    "Web Server",
    "MSP",
    "Database",
    "OS"
  ];
  
  var brandId = Math.floor(Math.random() * brands.length);
  var brand = brands[brandId];
  var applicationId = Math.floor(Math.random() * applications.length);
  var application = applications[applicationId];
  
  return brand + " " + application;
  
}