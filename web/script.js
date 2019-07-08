require('jquery');
require('bootstrap');

function getDesiredProperties() {
  return {
    "volume": {
      "volumeValue": parseFloat(jQuery('#dpVolumeValue').val()),
      "volumeUnit": jQuery('#dpVolumeUnit').text()
    },
    "lifetime": parseFloat(jQuery('#dpLifetime').val()),
    "dailyChanges": parseFloat(jQuery('#dpDailyChanges').val()) / 100,
    "contentType": jQuery('#dpContentType').val(),
    "accessType": jQuery('#dpAccessType').val(),
    "dailyReadVolume": parseFloat(jQuery('#dpDailyReadVolume').val()) / 100,
    "repairWithin": jQuery('#dpRepairWithin').val(),
    "repairTimes": jQuery('#dpRepairTimes').val(),
    "tapeBackup": jQuery('#dpRepairTimes').val() == 'yes',
    "backupFrequency": jQuery('#dpBackupFrequency').val(),
    "backupHistory": jQuery('#dpBackupHistory').val(),
    "unavailability": jQuery('#dpUnavailability').val(),
    "securityLevel": jQuery('#dpSecurityLevel').val(),
    "whoCanAccess": jQuery('#dpWhoCanAccess').val(),
    "sharedInfastructure": jQuery('#dpSharedInfrastracture').val() == 'yes'
  }
}

function getConfigVariables() {
  return {
    "hddCost": parseFloat(jQuery('#cvHDDCost').val()),
    "hddSize": parseFloat(jQuery('#cvHDDSize').val()),
    "hddPower": parseFloat(jQuery('#cvHDDPower').val()),
    "electricityCost": parseFloat(jQuery('#cvElectricityCost').val()),
    "powerEfficiency": parseFloat(jQuery('#cvPowerEfficiency').val()) / 100,
    "storageServerCost": parseFloat(jQuery('#cvStorageServer').val()),
    "storageRackCostPerYear": parseFloat(jQuery('#cvStorageRack').val()),
    "storagePower": parseFloat(jQuery('#cvServerPower').val()),
    "lifetimeHDD": parseFloat(jQuery('#cvLifetimeHDD').val()),
    "lifetimeServer": parseFloat(jQuery('#cvLifetimeServer').val()),
    "tapeSpeed": parseFloat(jQuery('#cvTapeSpeed').val()),
    "tapeCapacity": parseFloat(jQuery('#cvTapeCapacity').val()),
    "tapeCost": parseFloat(jQuery('#cvTapeCost').val()),
    "tapeDriveCost": parseFloat(jQuery('#cvTapeDriveCost').val()),
    "tapeRobotCost": parseFloat(jQuery('#cvTapeRobotCost').val()),
    "costMHR": parseFloat(jQuery('#cvCostMHR').val()),
    "networkCostPerTB": parseFloat(jQuery('#cvNetworkCost').val()),
    "firewallCost": parseFloat(jQuery('#cvFirewallCost').val()),
    "firewallMaintenance": parseFloat(jQuery('#cvFirewallMaintenance').val()),
    "networkPortRentCost": parseFloat(jQuery('#cvNetworkPortRent').val()),
    "upsCostPerWattMonth": parseFloat(jQuery('#cvUPS').val())
  }
}

function getInputs() {
  return {
    'desiredProperties': getDesiredProperties(),
    'configVariables': getConfigVariables()
  }
}


jQuery('body').ready(function(){
  jQuery('#dpVolumeUnitChange').click(function(){
    jQuery('#dpVolumeUnit').text(jQuery(this).text());
  });

  jQuery('#btnCalculate').click(function(){
    console.log('Sending inputs to server calculation ...');
    var inputs = getInputs();
    console.log(inputs);
    jQuery.ajax({
      type: "POST",
      data: JSON.stringify(inputs),
      success: function(data) {
        console.log('Received result from server ...');
        console.log(data);
        $('#resultJSON').text(JSON.stringify(data, null, 2));
        $('#resultJSON').each(function(i, e) {hljs.highlightBlock(e)});
      },
      error: function(err) {
        console.log('Error while communicating with server ...');
        console.log(err);
        $('#result').text("Wild error occured!");
      }
    });
  });
});
