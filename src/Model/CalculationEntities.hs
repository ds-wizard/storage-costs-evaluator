module Model.CalculationEntities where

import Control.Lens

import Model.Common
import Model.Inputs

data StorageDrives = StorageDrives
  { _storageDrivesVolume :: Double -- TB
  , _storageDrivesBackup :: Double -- TB
  , _storageDrivesRedundancy :: Double -- count
  , _storageDrivesRawVolume :: Double -- TB
  , _storageDrivesSpaceNeeded :: Double -- TB
  , _storageDrivesReplacements :: Double -- Drives/Drive
  , _storageDrivesTotalDriveCosts :: Double -- Euro
  , _storageDrivesPowerCosts :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''StorageDrives

data StorageServers = StorageServers
  { _storageServersDrives :: Double -- count
  , _storageServersShared :: Bool
  , _storageServersServers :: Double -- count
  , _storageServersRacks :: Double -- count
  , _storageServersReplacements :: Double -- Servers/Server
  , _storageServersServersCosts :: Double -- Euro
  , _storageServersRacksCosts :: Double -- Euro
  , _storageServersPowerCosts :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''StorageServers

data Networking = Networking
  { _networkingFirewalls :: Bool
  , _networkingBandwidthBackup :: Double -- TB
  , _networkingBandwidthOut :: Double -- TB/day
  , _networkingNetworkPorts :: Double -- Euro
  , _networkingBandwidthCosts :: Double -- Euro
  , _networkingFirewallCosts :: Double -- Euro
  , _networkingFirewallSetup :: Double -- Euro
  , _networkingFirewallMaintenance :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''Networking

data TapeBackup = TapeBackup
  { _tapeBackupRestoreTime :: Double -- hours
  , _tapeBackupAcceptableRepairTime :: Double -- hours
  , _tapeBackupSharedTape :: Bool
  , _tapeBackupDailyBackup :: Double -- TB
  , _tapeBackupBackupTime :: Double -- hours
  , _tapeBackupTapes :: Double -- count
  , _tapeBackupDrives :: Double -- count
  , _tapeBackupTapeRobot :: Bool
  , _tapeBackupTapeOperator :: Double -- hours
  , _tapeBackupNeedTapeIndex :: Bool
  , _tapeBackupTotalTapeCosts :: Double -- Euro
  , _tapeBackupTotalDriveCosts :: Double -- Euro
  , _tapeBackupTotalRobotCosts :: Double -- Euro
  , _tapeBackupOperatorCosts :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''TapeBackup

data Setup = Setup
  { _setupOperator :: Double -- hours
  , _setupOperatorCosts :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''Setup

data IncidentResponse = IncidentResponse
  { _indicentResponseFrequency :: Double -- per year
  , _indicentResponseOperatorCosts :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''IncidentResponse

data UninterruptiblePowerSupplies = UninterruptiblePowerSupplies
  { _uninterruptiblePowerSuppliesTotalCosts :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''UninterruptiblePowerSupplies

data StorageCosts = StorageCosts
  { _storageCostsStorageDrives :: StorageDrives
  , _storageCostsStorageServers :: StorageServers
  , _storageCostsNetworking :: Networking
  , _storageCostsTapeBackup :: TapeBackup
  , _storageCostsSetup :: Setup
  , _storageCostsIncidentResponse :: IncidentResponse
  , _storageCostsStorageUninterruptiblePowerSupplies :: UninterruptiblePowerSupplies
  , _storageCostsTotal :: Double -- Euro
  , _storageCostsPerYear :: Double -- Euro
  } deriving (Show, Read, Eq)

makeFields ''StorageCosts

data Result = Result
  { _resultInputs :: Inputs
  , _resultStorageCosts :: StorageCosts
  } deriving (Show, Read, Eq)

makeFields ''Result
