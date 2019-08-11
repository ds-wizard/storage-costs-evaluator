require('bootstrap');

import '@fortawesome/fontawesome-free/js/fontawesome';
import '@fortawesome/fontawesome-free/js/solid';
import '@fortawesome/fontawesome-free/js/regular';
//import '@fortawesome/fontawesome-free/js/brands';

var specs = require('./specs.js');


function forEachWithClassName(classname, func) {
  var elements =  document.getElementsByClassName(classname);
  for (var i = 0; i < elements.length; i++) {
    func(elements[i]);
  }
}

// TODO: nicer mapping with API request object
function getDesiredProperties() {
  function getDesiredProperty(id, what='value'){
    return document.getElementById(what + '-properties-' + id);
  }

  return {
    "volume": {
      "volumeValue": parseFloat(getDesiredProperty('volume').value),
      "volumeUnit": getDesiredProperty('volume', 'unit').value
    },
    "lifetime": parseFloat(getDesiredProperty('lifetime').value),
    "dailyChanges": parseFloat(getDesiredProperty('dailyChanges').value) / 100,
    "contentType": getDesiredProperty('contentType').value,
    "accessType": getDesiredProperty('accessType').value,
    "dailyReadVolume": parseFloat(getDesiredProperty('dailyReadVolume').value) / 100,
    "repairWithin": getDesiredProperty('repairWithin').value,
    "repairTimes": getDesiredProperty('repairTimes').value,
    "tapeBackup": getDesiredProperty('repairTimes').value == 'yes',
    "backupFrequency": getDesiredProperty('backupFrequency').value,
    "backupHistory": getDesiredProperty('backupHistory').value,
    "unavailability": getDesiredProperty('unavailability').value,
    "securityLevel": getDesiredProperty('securityLevel').value,
    "whoCanAccess": getDesiredProperty('whoCanAccess').value,
    "sharedInfastructure": getDesiredProperty('sharedInfrastracture').value == 'yes'
  }
}

function getConfigVariables() {
  function getConfigVariable(id, what='value'){
    return document.getElementById(what + '-configuration-' + id);
  }
  return {
    "hddCost": parseFloat(getConfigVariable('hddCost').value),
    "hddSize": parseFloat(getConfigVariable('hddSize').value),
    "hddPower": parseFloat(getConfigVariable('hddPower').value),
    "electricityCost": parseFloat(getConfigVariable('electricityCost').value),
    "powerEfficiency": parseFloat(getConfigVariable('powerEfficiency').value) / 100,
    "storageServerCost": parseFloat(getConfigVariable('storageServer').value),
    "storageRackCostPerYear": parseFloat(getConfigVariable('storageRack').value),
    "storagePower": parseFloat(getConfigVariable('serverPower').value),
    "lifetimeHDD": parseFloat(getConfigVariable('lifetimeHDD').value),
    "lifetimeServer": parseFloat(getConfigVariable('lifetimeServer').value),
    "tapeSpeed": parseFloat(getConfigVariable('tapeSpeed').value),
    "tapeCapacity": parseFloat(getConfigVariable('tapeCapacity').value),
    "tapeCost": parseFloat(getConfigVariable('tapeCost').value),
    "tapeDriveCost": parseFloat(getConfigVariable('tapeDriveCost').value),
    "tapeRobotCost": parseFloat(getConfigVariable('tapeRobotCost').value),
    "costMHR": parseFloat(getConfigVariable('costMHR').value),
    "networkCostPerTB": parseFloat(getConfigVariable('networkCost').value),
    "firewallCost": parseFloat(getConfigVariable('firewallCost').value),
    "firewallMaintenance": parseFloat(getConfigVariable('firewallMaintenance').value),
    "networkPortRentCost": parseFloat(getConfigVariable('networkPortRent').value),
    "upsCostPerWattMonth": parseFloat(getConfigVariable('ups').value)
  }
}

function getInputs() {
  return {
    'desiredProperties': getDesiredProperties(),
    'configVariables': getConfigVariables()
  }
}

function formatTotal(x) {
  return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, " ");
}

function processResult(result) {
  var storageCosts = result.storageCosts;
  for (var category in specs.resultMappings) {
    var catClass = specs.resultMappings[category]["_classname"];

    for (var item in specs.resultMappings[category]) {
      if (item[0] == '_') continue;
      var itemClass = specs.resultMappings[category][item]["_classname"];
      forEachWithClassName('value-' + catClass + '-' + itemClass, function(e) {
        var result = storageCosts[category][item];
        
        var transforms = specs.resultMappings[category][item]["_transform"];
        for (var i = 0; i < transforms.length; i++) {
          result = transforms[i](result);
        }

        e.innerText = result;
      });
    }

    var sum = 0;
    var subtotals = specs.resultMappings[category]['_total'];
    for (var i = 0; i < subtotals.length; i++) {
      sum += parseFloat(storageCosts[category][subtotals[i]]);
    }
    forEachWithClassName('total-value-' + catClass, function(e) {
      e.innerText = formatTotal(Math.round(sum));
    });

    forEachWithClassName('total-lifetime', function(e) {
      e.innerText = formatTotal(Math.round(storageCosts['total']));
    });

    forEachWithClassName('total-perYear', function(e) {
      e.innerText = formatTotal(Math.round(storageCosts['perYear']));
    });
  }
}


var inputsTemplate = require("ejs-compiled-loader!./parts/properties.ejs");
var inputTemplate = require("ejs-compiled-loader!./parts/input.ejs");
document.getElementById("x-inputs").innerHTML = inputsTemplate({
  'inputs': specs.inputs,
  'inputTemplate': inputTemplate
});

var resultsTemplate = require("ejs-compiled-loader!./parts/results.ejs");
var resultTemplate = require("ejs-compiled-loader!./parts/result.ejs");
document.getElementById("x-results").innerHTML = resultsTemplate({
  'results': specs.results,
  'resultTemplate': resultTemplate
});

function syncSharedInputs() {
  if (!this.hasAttribute("data-share")) return;
  var suffixID = this.getAttribute("data-share");
  var value = this.value;
  document.getElementById('value-'+suffixID).value = value;
  document.getElementById('range-'+suffixID).value = value;
}
forEachWithClassName('shared-input', function(e) {
  e.addEventListener('change', syncSharedInputs);
  e.addEventListener('input', syncSharedInputs);
});

function toggleResults() {
  var target = document.getElementById('x-results');
  if (target.classList.contains('show')) {
    target.classList.remove('show');
    this.setAttribute('aria-expanded', false);
  } else {
    target.classList.add('show');
    this.setAttribute('aria-expanded', true);
  }
}
document.getElementById('toggleResults').addEventListener('click', toggleResults);

function evaluate() {
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
}

forEachWithClassName('evaluator-input', function(e) {
  e.addEventListener('change', evaluate);
});

evaluate();
