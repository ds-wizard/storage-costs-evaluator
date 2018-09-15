module Model.JSON.CalculationEntities where

import Data.Aeson
import Control.Monad

import Model.CalculationEntities
import Model.JSON.Util


instance ToJSON StorageDrives where
  toJSON StorageDrives {..} =
    object
      [ "drivesVolume" .= _storageDrivesVolume
      , "backup" .= _storageDrivesBackup
      , "redundancy" .= _storageDrivesRedundancy
      , "rawVolume" .= _storageDrivesRawVolume
      , "spaceNeeded" .= _storageDrivesSpaceNeeded
      , "replacements" .= _storageDrivesReplacements
      , "driveCosts" .= _storageDrivesTotalDriveCosts
      , "powerCosts" .= _storageDrivesPowerCosts
      ]

instance ToJSON StorageServers where
  toJSON StorageServers {..} =
    object
      [ "drives" .= _storageServersDrives
      , "shared" .= _storageServersShared
      , "servers" .= _storageServersServers
      , "racks" .= _storageServersRacks
      , "replacements" .= _storageServersReplacements
      , "serversCosts" .= _storageServersServersCosts
      , "racksCosts" .= _storageServersRacksCosts
      , "powerCosts" .= _storageServersPowerCosts
      ]

instance ToJSON Networking where
  toJSON Networking {..} =
    object
      [ "drives" .= _networkingFirewalls
      , "backup" .= _networkingBandwidthBackup
      , "bandwidthOut" .= _networkingBandwidthOut
      , "networkPorts" .= _networkingNetworkPorts
      , "bandwidthCosts" .= _networkingBandwidthCosts
      , "firewallCosts" .= _networkingFirewallCosts
      , "firewallSetup" .= _networkingFirewallSetup
      , "firewallMaintenance" .= _networkingFirewallMaintenance
      ]

instance ToJSON TapeBackup where
  toJSON TapeBackup {..} =
    object
      [ "restoreTime" .= _tapeBackupRestoreTime
      , "repairTime" .= _tapeBackupAcceptableRepairTime
      , "sharedTape" .= _tapeBackupSharedTape
      , "dailyBackup" .= _tapeBackupDailyBackup
      , "backupTime" .= _tapeBackupBackupTime
      , "tapes" .= _tapeBackupTapes
      , "drives" .= _tapeBackupDrives
      , "tapeRobot" .= _tapeBackupTapeRobot
      , "tapeOperator" .= _tapeBackupTapeOperator
      , "tapeIndex" .= _tapeBackupNeedTapeIndex
      , "tapeCosts" .= _tapeBackupTotalTapeCosts
      , "driveCosts" .= _tapeBackupTotalDriveCosts
      , "robotCosts" .= _tapeBackupTotalRobotCosts
      , "operatorCosts" .= _tapeBackupOperatorCosts
      ]

instance ToJSON Setup where
  toJSON Setup {..} =
    object
      [ "operator" .= _setupOperator
      , "operatorCosts" .= _setupOperatorCosts
      ]

instance ToJSON IncidentResponse where
  toJSON IncidentResponse {..} =
    object
      [ "frequency" .= _indicentResponseFrequency
      , "operatorCosts" .= _indicentResponseOperatorCosts
      ]

instance ToJSON UninterruptiblePowerSupplies where
  toJSON UninterruptiblePowerSupplies {..} =
    object
      [ "totalCosts" .= _uninterruptiblePowerSuppliesTotalCosts
      ]

instance ToJSON StorageCosts where
  toJSON StorageCosts {..} =
    object
      [ "storageDrives" .= _storageCostsStorageDrives
      , "storageServers" .= _storageCostsStorageServers
      , "networking" .= _storageCostsNetworking
      , "tapeBackup" .= _storageCostsTapeBackup
      , "costsSetup" .= _storageCostsSetup
      , "indicentResponse" .= _storageCostsIncidentResponse
      , "powerSupplies" .= _storageCostsStorageUninterruptiblePowerSupplies
      , "total" .= _storageCostsTotal
      , "perYear" .= _storageCostsPerYear
      ]
