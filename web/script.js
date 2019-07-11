var specs = require('./specs.js');

function getDesiredProperties() {
  return {
    "volume": {
      "volumeValue": parseFloat(document.getElementById('dpVolumeValue').value),
      "volumeUnit": document.getElementById('dpVolumeUnit').innerText
    },
    "lifetime": parseFloat(document.getElementById('dpLifetime').value),
    "dailyChanges": parseFloat(document.getElementById('dpDailyChanges').value) / 100,
    "contentType": document.getElementById('dpContentType').value,
    "accessType": document.getElementById('dpAccessType').value,
    "dailyReadVolume": parseFloat(document.getElementById('dpDailyReadVolume').value) / 100,
    "repairWithin": document.getElementById('dpRepairWithin').value,
    "repairTimes": document.getElementById('dpRepairTimes').value,
    "tapeBackup": document.getElementById('dpRepairTimes').value == 'yes',
    "backupFrequency": document.getElementById('dpBackupFrequency').value,
    "backupHistory": document.getElementById('dpBackupHistory').value,
    "unavailability": document.getElementById('dpUnavailability').value,
    "securityLevel": document.getElementById('dpSecurityLevel').value,
    "whoCanAccess": document.getElementById('dpWhoCanAccess').value,
    "sharedInfastructure": document.getElementById('dpSharedInfrastracture').value == 'yes'
  }
}

function getConfigVariables() {
  return {
    "hddCost": parseFloat(document.getElementById('cvHDDCost').value),
    "hddSize": parseFloat(document.getElementById('cvHDDSize').value),
    "hddPower": parseFloat(document.getElementById('cvHDDPower').value),
    "electricityCost": parseFloat(document.getElementById('cvElectricityCost').value),
    "powerEfficiency": parseFloat(document.getElementById('cvPowerEfficiency').value) / 100,
    "storageServerCost": parseFloat(document.getElementById('cvStorageServer').value),
    "storageRackCostPerYear": parseFloat(document.getElementById('cvStorageRack').value),
    "storagePower": parseFloat(document.getElementById('cvServerPower').value),
    "lifetimeHDD": parseFloat(document.getElementById('cvLifetimeHDD').value),
    "lifetimeServer": parseFloat(document.getElementById('cvLifetimeServer').value),
    "tapeSpeed": parseFloat(document.getElementById('cvTapeSpeed').value),
    "tapeCapacity": parseFloat(document.getElementById('cvTapeCapacity').value),
    "tapeCost": parseFloat(document.getElementById('cvTapeCost').value),
    "tapeDriveCost": parseFloat(document.getElementById('cvTapeDriveCost').value),
    "tapeRobotCost": parseFloat(document.getElementById('cvTapeRobotCost').value),
    "costMHR": parseFloat(document.getElementById('cvCostMHR').value),
    "networkCostPerTB": parseFloat(document.getElementById('cvNetworkCost').value),
    "firewallCost": parseFloat(document.getElementById('cvFirewallCost').value),
    "firewallMaintenance": parseFloat(document.getElementById('cvFirewallMaintenance').value),
    "networkPortRentCost": parseFloat(document.getElementById('cvNetworkPortRent').value),
    "upsCostPerWattMonth": parseFloat(document.getElementById('cvUPS').value)
  }
}

function getInputs() {
  return {
    'desiredProperties': getDesiredProperties(),
    'configVariables': getConfigVariables()
  }
}


var inputsTemplate = require("ejs-compiled-loader!./parts/inputs.ejs");
document.getElementById("x-inputs").innerHTML = inputsTemplate();

var resultsTemplate = require("ejs-compiled-loader!./parts/results.ejs");
var resultTemplate = require("ejs-compiled-loader!./parts/result.ejs");
document.getElementById("x-results").innerHTML = resultsTemplate({
  'results': specs.results,
  'resultTemplate': resultTemplate
});

// Units of volume
var elems = document.getElementsByClassName('dpVolumeUnitChange');
for (var i = 0; i < elems.length; i++) {
  elems[i].onclick = function(){
    alert(this.innerText);
    document.getElementById('dpVolumeUnit').innerText = this.innerText;
  };
}

// TODO: Accordion with Vanilla JS (Bootstrap)

// TODO: Dropdown with Vanilly JS (Bootstrap)

// Calculation
document.getElementById('btnCalculate').onclick = function(){
  console.log('Sending inputs to server calculation ...');
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.onload = function() {
      if (xhr.status === 200) {
        console.log('Received result from server');
        document.getElementById('resultJSON').innerText = JSON.stringify(JSON.parse(xhr.responseText), null, 2);
      } else {
        console.log('Request failed - returned status of ' + xhr.status);
        document.getElementById('resultJSON').innerText = JSON.stringify(getInputs(), null, 2);
      }
  };
  xhr.send(JSON.stringify(getInputs()));
};

