module Specs.Data.Inputs where

import Model.Inputs
import Model.Common


defDP = DesiredProperties
  { _desiredPropertiesVolume = Volume 10 TB
  , _desiredPropertiesLifetime = 10
  , _desiredPropertiesDailyChanges = 0.1
  , _desiredPropertiesContentType = ManySmallFiles
  , _desiredPropertiesAccessType = OneFileOnRequest
  , _desiredPropertiesDailyReadVolume = 0.1
  , _desiredPropertiesRepairWithin = Days
  , _desiredPropertiesRepairTimes = D5H8
  , _desiredPropertiesTapeBackup = True
  , _desiredPropertiesBackupFrequency = Days
  , _desiredPropertiesBackupHistory = Months
  , _desiredPropertiesUnavailability = Day
  , _desiredPropertiesSecurityLevel = OpenData
  , _desiredPropertiesWhoCanAccess = OpenAccess
  , _desiredPropertiesSharedInfrastracture = True
  }

defCV = ConfigVariables
  { _configVariablesHDDCost = 30 -- Euro/TB
  , _configVariablesHDDSize = 10 -- TB
  , _configVariablesHDDPower = 1.5 -- Watt/TB
  , _configVariablesElectricityCost = 0.14 -- Euro/kWh
  , _configVariablesPowerEfficiency = 0.7
  , _configVariablesStorageServer = 5000 -- Euro
  , _configVariablesStorageRack = 2000 -- Euro/year
  , _configVariablesServerPower = 200 -- Watt
  , _configVariablesLifetimeHDD = 3 -- years
  , _configVariablesLifetimeServer = 5 -- years
  , _configVariablesTapeSpeed = 432 -- GB/hour
  , _configVariablesTapeCapacity = 15 -- TB
  , _configVariablesTapeCost = 150 -- Euro
  , _configVariablesTapeDriveCost = 3000 -- Euro
  , _configVariablesTapeRobotCost = 50000 -- Euro
  , _configVariablesCostMHR = 70 -- Euro
  , _configVariablesNetworkCost = 0.5 -- Euro/TB
  , _configVariablesFirewallCost = 1000 -- Euro
  , _configVariablesFirewallMaintenance = 0.1 -- Hours/month
  , _configVariablesNetworkPortRent = 10 -- Euro/month
  , _configVariablesUPS = 0.1 -- Euro/Watt/month
  }
