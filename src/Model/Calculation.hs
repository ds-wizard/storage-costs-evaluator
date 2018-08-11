module Model.Calculation where

import Control.Lens

import Model.CalculationEntities
import Model.Common
import Model.Inputs


doubleCeiling = fromIntegral . ceiling


calculateCosts :: ConfigVariables -> DesiredProperties -> StorageCosts
calculateCosts cv dp = StorageCosts
  { _storageCostsStorageDrives = StorageDrives
    { _storageDrivesVolume = sdVolume
    , _storageDrivesBackup = sdBackup
    , _storageDrivesRedundancy = sdRedundancy
    , _storageDrivesRawVolume = sdRawVolume
    , _storageDrivesSpaceNeeded = sdSpaceNeeded
    , _storageDrivesReplacements = sdReplacements
    , _storageDrivesTotalDriveCosts = sdTotalDriveCosts
    , _storageDrivesPowerCosts = sdPowerCosts
    }
  , _storageCostsStorageServers = StorageServers
    { _storageServersDrives = ssDrives
    , _storageServersShared = ssShared
    , _storageServersServers = ssServers
    , _storageServersRacks = ssRacks
    , _storageServersReplacements = ssReplacements
    , _storageServersServersCosts = ssServersCosts
    , _storageServersRacksCosts = ssRacksCosts
    , _storageServersPowerCosts = ssPowerCosts
    }
  , _storageCostsNetworking = Networking
    { _networkingFirewalls = nFirewalls
    , _networkingBandwidthBackup = nBandwidthBackup
    , _networkingBandwidthOut = nBandwidthOut
    , _networkingNetworkPorts = nNetworkPorts
    , _networkingBandwidthCosts = nBandwidthCosts
    , _networkingFirewallCosts = nFirewallCosts
    , _networkingFirewallSetup = nFirewallSetup
    , _networkingFirewallMaintenance = nFirewallMaintenance
    }
  , _storageCostsTapeBackup = TapeBackup
    { _tapeBackupRestoreTime = tbRestoreTime
    , _tapeBackupAcceptableRepairTime = tbAcceptableRepairTime
    , _tapeBackupSharedTape = tbSharedTape
    , _tapeBackupDailyBackup = tbDailyBackup
    , _tapeBackupBackupTime = tbBackupTime
    , _tapeBackupTapes = tbTapes
    , _tapeBackupDrives = tbDrives
    , _tapeBackupTapeRobot = tbTapeRobot
    , _tapeBackupTapeOperator = tbTapeOperator
    , _tapeBackupNeedTapeIndex = tbNeedTapeIndex
    , _tapeBackupTotalTapeCosts = tbTotalTapeCosts
    , _tapeBackupTotalDriveCosts = tbTotalDriveCosts
    , _tapeBackupTotalRobotCosts = tbTotalRobotCosts
    , _tapeBackupOperatorCosts = tbOperatorCosts
    }
  , _storageCostsSetup = Setup
    { _setupOperator = sOperator
    , _setupOperatorCosts = sOperatorCosts
    }
  , _storageCostsIncidentResponse = IncidentResponse
    { _indicentResponseFrequency = irFrequency
    , _indicentResponseOperatorCosts = irOperatorCosts
    }
  , _storageCostsStorageUninterruptiblePowerSupplies = UninterruptiblePowerSupplies
    { _uninterruptiblePowerSuppliesTotalCosts = upsTotalCosts
    }
  , _storageCostsTotal = totalCosts
  , _storageCostsPerYear = perYearCosts
  }
  where
    sdVolume = toTB (dp ^. volume)
    sdBackup = if toHours (dp ^. backupFrequency) < 24
               then (dp ^. dailyChanges) * sdVolume * min (toHours (dp ^. backupHistory) / 24) 30
               else 0
    sdRedundancy = (if tbRestoreTime > tbAcceptableRepairTime then 2 else 1) + (if (dp ^. dailyChanges) == 0 then 0 else 1)
    sdRawVolume = sdRedundancy * (sdVolume + sdBackup)
    sdSpaceNeeded = (if (dp ^. dailyChanges) == 0 then 1 else 1.25) * sdRawVolume
    sdReplacements = max 1 ((dp ^. lifetime) / (cv ^. lifetimeHDD))
    sdTotalDriveCosts = (cv ^. hDDCost) * sdSpaceNeeded * sdReplacements * (if (dp ^. accessType) == HighPerformance then 2 else 1)
    sdPowerCosts = sdSpaceNeeded * (cv ^. electricityCost) * whTokWHperYear (cv ^. hDDPower) * (dp ^. lifetime) / (cv ^. powerEfficiency)
    ssDrives = doubleCeiling (sdSpaceNeeded / (cv ^. hDDSize))
    ssShared = canShare && ssDrives < 30  -- Q: what is 30?
    ssServers = (if ssShared then ssDrives / 40 else doubleCeiling (ssDrives / 40)) * (if tbAcceptableRepairTime < 0.05 then 2 else 1)
    ssRacks = ssServers / 6 -- Q: rack size 6?
    ssReplacements = (dp ^. lifetime) / (cv ^. lifetimeServer)
    ssServersCosts = ssServers * (cv ^. storageServer) * ssReplacements * (if (dp ^. accessType) == HighPerformance then 2 else 1)
    ssRacksCosts = ssRacks * (cv ^. storageRack) * (dp ^. lifetime) * (if (dp ^. securityLevel) == PrivacySensitive then 2 else 1)
    ssPowerCosts = ssServers * (cv ^. electricityCost) * whTokWHperYear (cv ^. serverPower) * (dp ^. lifetime) / (cv ^. powerEfficiency)
    nFirewalls = (dp ^. securityLevel) == PrivacySensitive
    nBandwidthBackup = tbDailyBackup * (sdRedundancy - 1 + (if (dp ^. tapeBackup) == Yes then 1 else 0))
    nBandwidthOut = (dp ^. dailyReadVolume) * sdVolume
    nNetworkPorts = ((if ssShared then 0 else max sdRedundancy ssServers * (cv ^. networkPortRent)) + (if tbSharedTape then 0 else 1)) * 12 * (dp ^. lifetime)
    nBandwidthCosts = (nBandwidthBackup + nBandwidthOut) * (cv ^. networkCost) * 365 * (dp ^. lifetime)
    nFirewallCosts = if nFirewalls then (cv ^. firewallCost) * sdRedundancy else 0
    nFirewallSetup = if nFirewalls then (2 + sdRedundancy) * (cv ^. costMHR) else 0
    nFirewallMaintenance = if nFirewalls then (cv ^. costMHR) * (cv ^. firewallMaintenance) * 12 * (dp ^. lifetime) else 0
    tbRestoreTime = 1000 * sdVolume / (cv ^. tapeSpeed)
    tbAcceptableRepairTime = 4 * toHours (dp ^. repairWithin)
    tbSharedTape = canShare && tbBackupTime < 12
    tbDailyBackup = (dp ^. dailyChanges) * sdVolume
    tbBackupTime = (1000 * tbDailyBackup / (cv ^. tapeSpeed)) * (if (dp ^. contentType) == ManySmallFiles then 2 else 1)
    tbTapes = (if (dp ^. backupHistory) == Days then 5 else if (dp ^. backupHistory) == Weeks then 10 else 16) +
              (dp ^. lifetime) * (if (dp ^. backupHistory) == Years then 1 else 0) *
              (if tbSharedTape then tbDailyBackup / (cv ^. tapeCapacity) else doubleCeiling (tbDailyBackup/(cv ^. tapeCapacity)))
    tbDrives = if tbSharedTape then tbBackupTime / 24 else doubleCeiling (tbBackupTime / 24)
    tbTapeRobot = tbDailyBackup > (cv ^. tapeCapacity)
    tbTapeOperator = (if tbDailyBackup > 0 then (dp ^. lifetime) * 200 * 0.05 else 0) *
                     (if tbTapeRobot then 1 else tbDrives) -- Q: 200? 0.05?
    tbNeedTapeIndex = tbTapes > 5 -- Q: why 5?
    tbTotalTapeCosts = if (dp ^. tapeBackup) == Yes then tbTapes * (cv ^. tapeCost) else 0
    tbTotalDriveCosts = if (dp ^. tapeBackup) == Yes then tbDrives * (cv ^. tapeDriveCost) else 0
    tbTotalRobotCosts = if tbTapeRobot then cv ^. tapeRobotCost else 0
    tbOperatorCosts = tbTapeOperator * (cv ^. costMHR)
    sOperator = ssDrives * 0.1 + ssServers * 2 + ssRacks * 4 + tbDrives * 5 + 8
    sOperatorCosts = sOperator * (cv ^. costMHR)
    irFrequency = (ssDrives + ssServers + tbDrives) / 3
    irOperatorCosts = irFrequency * (dp ^. lifetime) * (cv ^. costMHR) * 0.5 *
                      (if (dp ^. securityLevel) == PrivacySensitive then 1.5 else 1) *
                      (if (dp ^. repairTimes) == D7H24 then 3 else 1)
    upsTotalCosts = (sdSpaceNeeded * (cv ^. electricityCost) + ssServers * (cv ^. serverPower)) * -- Q: no HDD power?
                    (cv ^. uPS) * 12 * (dp ^. lifetime) *
                    (if (dp ^. unavailability) == Hour then 3 else 1)
    totalCosts = sum [ sdTotalDriveCosts, sdPowerCosts
                     , ssServersCosts, ssRacksCosts, ssPowerCosts
                     , nNetworkPorts, nBandwidthCosts, nFirewallCosts, nFirewallSetup, nFirewallMaintenance
                     , tbTotalTapeCosts, tbTotalDriveCosts, tbTotalRobotCosts, tbOperatorCosts
                     , sOperatorCosts, irOperatorCosts, upsTotalCosts
                     ]
    perYearCosts = totalCosts / (dp ^. lifetime) / sdVolume
    canShare = (dp ^. sharedInfrastracture) == Yes && (dp ^. securityLevel) /= PrivacySensitive
