module Model.JSON.Inputs where

import Data.Aeson
import Data.Default
import Data.Maybe
import Control.Monad

import Model.Inputs
import Model.JSON.Common
import Model.JSON.Util


instance ToJSON DesiredProperties where
  toJSON DesiredProperties {..} =
    object
      [ "volume" .= _desiredPropertiesVolume
      , "lifetime" .= _desiredPropertiesLifetime
      , "dailyChanges" .= _desiredPropertiesDailyChanges
      , "contentType" .= show _desiredPropertiesContentType
      , "accessType" .= show _desiredPropertiesAccessType
      , "dailyReadVolume" .= _desiredPropertiesDailyReadVolume
      , "repairWithin" .= show _desiredPropertiesRepairWithin
      , "repairTimes" .= show _desiredPropertiesRepairTimes
      , "tapeBackup" .= _desiredPropertiesTapeBackup
      , "backupFrequency" .= show _desiredPropertiesBackupFrequency
      , "backupHistory" .= show _desiredPropertiesBackupHistory
      , "unavailability" .= show _desiredPropertiesUnavailability
      , "securityLevel" .= show _desiredPropertiesSecurityLevel
      , "whoCanAccess" .= show _desiredPropertiesWhoCanAccess
      , "sharedInfastructure" .= _desiredPropertiesSharedInfrastracture
      ]

instance FromJSON DesiredProperties where
  parseJSON (Object o) = do
    _desiredPropertiesVolume <- o .: "volume"
    _desiredPropertiesLifetime <- o .: "lifetime"
    _desiredPropertiesDailyChanges <- o .: "dailyChanges"
    _desiredPropertiesContentType <- parseWithRead o "contentType"
    _desiredPropertiesAccessType <- parseWithRead o "accessType"
    _desiredPropertiesDailyReadVolume <- o .: "dailyReadVolume"
    _desiredPropertiesRepairWithin <- parseWithRead o "repairWithin"
    _desiredPropertiesRepairTimes <- parseWithRead o "repairTimes"
    _desiredPropertiesTapeBackup <- o .: "tapeBackup"
    _desiredPropertiesBackupFrequency <- parseWithRead o "backupFrequency"
    _desiredPropertiesBackupHistory <- parseWithRead o "backupHistory"
    _desiredPropertiesUnavailability <- parseWithRead o "unavailability"
    _desiredPropertiesSecurityLevel <- parseWithRead o "securityLevel"
    _desiredPropertiesWhoCanAccess <- parseWithRead o "whoCanAccess"
    _desiredPropertiesSharedInfrastracture <- o .: "sharedInfastructure"
    return DesiredProperties {..}
  parseJSON _ = mzero


instance ToJSON ConfigVariables where
  toJSON ConfigVariables {..} =
    object
      [ "hddCost" .=  _configVariablesHDDCost
      , "hddSize" .= _configVariablesHDDSize
      , "hddPower" .= _configVariablesHDDPower
      , "electricityCost" .= _configVariablesElectricityCost
      , "powerEfficiency" .= _configVariablesPowerEfficiency
      , "storageServerCost" .= _configVariablesStorageServer
      , "storageRackCostPerYear" .= _configVariablesStorageRack
      , "storagePower" .= _configVariablesServerPower
      , "lifetimeHDD" .= _configVariablesLifetimeHDD
      , "lifetimeServer" .= _configVariablesLifetimeServer
      , "tapeSpeed" .= _configVariablesTapeSpeed
      , "tapeCapacity" .= _configVariablesTapeCapacity
      , "tapeCost" .= _configVariablesTapeCost
      , "tapeDriveCost" .= _configVariablesTapeDriveCost
      , "tapeRobotCost" .= _configVariablesTapeRobotCost
      , "costMHR" .= _configVariablesCostMHR
      , "networkCostPerTB" .= _configVariablesNetworkCost
      , "firewallCost" .= _configVariablesFirewallCost
      , "firewallMaintenance" .= _configVariablesFirewallMaintenance
      , "networkPortRentCost" .= _configVariablesNetworkPortRent
      , "upsCostPerWattMonth" .= _configVariablesUPS
      ]

instance FromJSON ConfigVariables where
  parseJSON (Object o) = do
    _configVariablesHDDCost <- o .: "hddCost"
    _configVariablesHDDSize <- o .: "hddSize"
    _configVariablesHDDPower <- o .: "hddPower"
    _configVariablesElectricityCost <- o .: "electricityCost"
    _configVariablesPowerEfficiency <- o .: "powerEfficiency"
    _configVariablesStorageServer <- o .: "storageServerCost"
    _configVariablesStorageRack <- o .: "storageRackCostPerYear"
    _configVariablesServerPower <- o .: "storagePower"
    _configVariablesLifetimeHDD <- o .: "lifetimeHDD"
    _configVariablesLifetimeServer <- o .: "lifetimeServer"
    _configVariablesTapeSpeed <- o .: "tapeSpeed"
    _configVariablesTapeCapacity <- o .: "tapeCapacity"
    _configVariablesTapeCost <- o .: "tapeCost"
    _configVariablesTapeDriveCost <- o .: "tapeDriveCost"
    _configVariablesTapeRobotCost <- o .: "tapeRobotCost"
    _configVariablesCostMHR <- o .: "costMHR"
    _configVariablesNetworkCost <- o .: "networkCostPerTB"
    _configVariablesFirewallCost <- o .: "firewallCost"
    _configVariablesFirewallMaintenance <- o .: "firewallMaintenance"
    _configVariablesNetworkPortRent <- o .: "networkPortRentCost"
    _configVariablesUPS <- o .: "upsCostPerWattMonth"
    return ConfigVariables {..}
  parseJSON _ = mzero

instance ToJSON Inputs where
  toJSON Inputs {..} =
    object
      [ "desiredProperties" .= _inputsDesiredProperties
      , "configVariables" .= _inputsConfigVariables
      ]

instance FromJSON Inputs where
  parseJSON (Object o) = do
    _inputsDesiredProperties <- o .: "desiredProperties"
    _inputsConfigVariables <- fromMaybe def <$> o .:? "configVariables"
    return Inputs {..}
  parseJSON _ = mzero
