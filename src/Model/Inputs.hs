module Model.Inputs where

import Data.Default
import Control.Lens

import Model.Common


data DesiredProperties = DesiredProperties
  { _desiredPropertiesVolume :: Volume
  , _desiredPropertiesLifetime :: Years
  , _desiredPropertiesDailyChanges :: Percentage
  , _desiredPropertiesContentType :: ContentType
  , _desiredPropertiesAccessType :: AccessType
  , _desiredPropertiesDailyReadVolume :: Percentage
  , _desiredPropertiesRepairWithin :: TimeHorizon
  , _desiredPropertiesRepairTimes :: BusinessHours
  , _desiredPropertiesTapeBackup :: Bool
  , _desiredPropertiesBackupFrequency :: TimeHorizon
  , _desiredPropertiesBackupHistory :: TimeHorizon
  , _desiredPropertiesUnavailability :: Unavailability
  , _desiredPropertiesSecurityLevel :: SecurityLevel
  , _desiredPropertiesWhoCanAccess :: AccessGroup
  , _desiredPropertiesSharedInfrastracture :: Bool
  } deriving (Show, Read, Eq)

makeFields ''DesiredProperties

data ConfigVariables = ConfigVariables
  { _configVariablesHDDCost :: Double -- Euro/TB
  , _configVariablesHDDSize :: Double -- TB
  , _configVariablesHDDPower :: Double -- Watt/TB
  , _configVariablesElectricityCost :: Double -- Euro/kWh
  , _configVariablesPowerEfficiency :: Percentage -- Percentage (0-1)
  , _configVariablesStorageServer :: Double -- Euro
  , _configVariablesStorageRack :: Double -- Euro/year
  , _configVariablesServerPower :: Double -- Watt
  , _configVariablesLifetimeHDD :: Double -- years
  , _configVariablesLifetimeServer :: Double -- years
  , _configVariablesTapeSpeed :: Double -- GB/hour
  , _configVariablesTapeCapacity :: Double -- TB
  , _configVariablesTapeCost :: Double -- Euro
  , _configVariablesTapeDriveCost :: Double -- Euro
  , _configVariablesTapeRobotCost :: Double -- Euro
  , _configVariablesCostMHR :: Double -- Euro
  , _configVariablesNetworkCost :: Double -- Euro/TB
  , _configVariablesFirewallCost :: Double -- Euro
  , _configVariablesFirewallMaintenance :: Double -- Hours/month
  , _configVariablesNetworkPortRent :: Double -- Euro/month
  , _configVariablesUPS :: Double -- Euro/Watt/month
  } deriving (Show, Read, Eq)

makeFields ''ConfigVariables

instance Default ConfigVariables where
  def = ConfigVariables
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
    , _configVariablesUPS = 0.2 -- Euro/Watt/month
    }
