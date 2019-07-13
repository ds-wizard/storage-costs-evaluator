var specs = require('./specs.js');


function forEachWithClassName(classname, func) {
  var elements =  document.getElementsByClassName(classname);
  for (var i = 0; i < elements.length; i++) {
    func(elements[i]);
  }
}

function getDesiredProperties() {
  return {
    "volume": {
      "volumeValue": parseFloat(document.getElementById('dpVolumeValue').value),
      "volumeUnit": document.getElementById('dpVolumeUnit').value
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

function processResult(result) {
  var storageCosts = result.storageCosts;
  for (category in specs.resultMappings) {
    var catClass = specs.resultMappings[category]["_classname"];

    for (item in specs.resultMappings[category]) {
      if (item[0] == '_') continue;
      var itemClass = specs.resultMappings[category][item];
      forEachWithClassName('value-' + catClass + '-' + itemClass, function(e) {
        // TODO: process value = transform, round, etc.
        e.innerText = storageCosts[category][item];
      });
    }

    var sum = 0;
    var subtotals = specs.resultMappings[category]['_total'];
    for (var i = 0; i < subtotals.length; i++) {
      sum += parseFloat(storageCosts[category][subtotals[i]]);
    }
    forEachWithClassName('total-value-' + catClass, function(e) {
      e.innerText = Math.round(sum);
    });

    forEachWithClassName('total-lifetime', function(e) {
      e.innerText = Math.round(storageCosts['total']);
    });

    forEachWithClassName('total-perYear', function(e) {
      e.innerText = Math.round(storageCosts['perYear']*100) / 100;
    });
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

// TODO: Accordion with Vanilla JS (Bootstrap)
forEachWithClassName('btn', function(button) {
  button.onclick = function() {
    var toggle = this.getAttribute('data-toggle');
    if (toggle == 'collapse') {
      var target = this.getAttribute('data-target').slice(1);
      document.getElementById(target).classList.toggle('show');
    }
  };
});

// Calculation
document.getElementById('btnCalculate').onclick = function(){
  console.log('Sending inputs to server calculation ...');
  var xhr = new XMLHttpRequest();
  xhr.open('POST', '/');
  xhr.setRequestHeader('Content-Type', 'application/json');
  xhr.onload = function() {
      if (xhr.status === 200) {
        console.log('Received result from server');
        processResult(JSON.parse(xhr.responseText));
      } else {
        console.log('Request failed - returned status of ' + xhr.status);
      }
  };
  xhr.send(JSON.stringify(getInputs()));
};

