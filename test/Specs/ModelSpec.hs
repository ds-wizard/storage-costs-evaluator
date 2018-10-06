module Specs.ModelSpec (spec) where

import Control.Lens
import Test.Hspec

import Specs.Data.Inputs

import Model.Calculation
import Model.CalculationEntities
import Model.Common
import Model.Inputs


rnd n f = fromInteger ( round $ f * (10^n)) / (10.0^^n)


spec :: Spec
spec = do
  describe "StorageDrives" $ do
    it "calculates volume in TB" $ do
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 7 TB}) ^. storageDrives . usableVolume `shouldBe` 7
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 10000 GB}) ^. storageDrives . usableVolume `shouldBe` 10
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 9 PB}) ^. storageDrives . usableVolume `shouldBe` 9000
    it "calculates daily backup in TB" $ do
      calculateCosts defCV defDP ^. storageDrives . backup `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Hours}) ^. storageDrives . backup `shouldBe` 30
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Seconds}) ^. storageDrives . backup `shouldBe` 30
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Hours, _desiredPropertiesVolume = Volume 20 TB}) ^. storageDrives . backup `shouldBe` 60
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Hours, _desiredPropertiesDailyChanges = 0.05}) ^. storageDrives . backup `shouldBe` 15
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Hours, _desiredPropertiesBackupHistory = Weeks}) ^. storageDrives . backup `shouldBe` 7
    it "calculates needed number of HDDs (redundancy)" $ do
      calculateCosts defCV defDP ^. storageDrives . redundancy `shouldBe` 2
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. storageDrives . redundancy `shouldBe` 1
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. storageDrives . redundancy `shouldBe` 3
      calculateCosts (defCV {_configVariablesTapeSpeed = 40})  defDP ^. storageDrives . redundancy `shouldBe` 3
    it "calculates raw volume" $ do
      calculateCosts defCV defDP ^. storageDrives . rawVolume `shouldBe` 20
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. storageDrives . rawVolume `shouldBe` 10
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. storageDrives . rawVolume `shouldBe` 150
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Minutes}) ^. storageDrives . rawVolume `shouldBe` 80
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Minutes, _desiredPropertiesBackupHistory = Weeks}) ^. storageDrives . rawVolume `shouldBe` 34
    it "calculates needed space" $ do
      calculateCosts defCV defDP ^. storageDrives . spaceNeeded `shouldBe` 25
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. storageDrives . spaceNeeded `shouldBe` 10
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. storageDrives . spaceNeeded `shouldBe` 3750
    it "calculates number of replacements" $ do
      calculateCosts defCV defDP ^. storageDrives . replacements `shouldBe` 10/3
      calculateCosts defCV (defDP {_desiredPropertiesLifetime = 30}) ^. storageDrives . replacements `shouldBe` 10
      calculateCosts (defCV {_configVariablesLifetimeHDD = 4}) defDP ^. storageDrives . replacements `shouldBe` 2.5
    it "calculates total drive costs" $ do
      calculateCosts defCV defDP ^. storageDrives . totalDriveCosts `shouldBe` 2500
      calculateCosts defCV (defDP {_desiredPropertiesAccessType = HighPerformance}) ^. storageDrives . totalDriveCosts `shouldBe` 5000
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. storageDrives . totalDriveCosts `shouldBe` 375000
      calculateCosts defCV (defDP {_desiredPropertiesBackupFrequency = Seconds}) ^. storageDrives . totalDriveCosts `shouldBe` 10000
      calculateCosts (defCV {_configVariablesLifetimeHDD = 4}) defDP ^. storageDrives . totalDriveCosts `shouldBe` 1875
      calculateCosts (defCV {_configVariablesHDDCost = 60}) defDP ^. storageDrives . totalDriveCosts `shouldBe` 5000
    it "calculates power costs (for lifetime)" $ do
      rnd 2 (calculateCosts defCV defDP ^. storageDrives . powerCosts) `shouldBe` 657
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 5}) ^. storageDrives . powerCosts) `shouldBe` 328.5
      rnd 2 (calculateCosts (defCV {_configVariablesElectricityCost = 0.7}) defDP ^. storageDrives . powerCosts) `shouldBe` 3285
      rnd 2 (calculateCosts (defCV {_configVariablesHDDPower = 2.2}) defDP ^. storageDrives . powerCosts) `shouldBe` 963.6
      rnd 2 (calculateCosts (defCV {_configVariablesPowerEfficiency = 1}) defDP ^. storageDrives . powerCosts) `shouldBe` 459.9
  describe "StorageServers" $ do
    it "calculates number of server drives" $ do
      calculateCosts defCV defDP ^. storageServers . drives `shouldBe` 3
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. storageServers . drives `shouldBe` 19
      calculateCosts (defCV {_configVariablesHDDSize = 5}) defDP ^. storageServers . drives `shouldBe` 5
      calculateCosts (defCV {_configVariablesHDDSize = 300}) defDP ^. storageServers . drives `shouldBe` 1
    it "calculates if can be shared" $ do
      calculateCosts defCV defDP ^. storageServers . shared `shouldBe` True
      calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. storageServers . shared `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. storageServers . shared `shouldBe` False
    it "calculates number of servers" $ do
      calculateCosts defCV defDP ^. storageServers . servers `shouldBe` 0.075
      calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. storageServers . servers `shouldBe` 1
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 42 TB}) ^. storageServers . servers `shouldBe` 0.4
      calculateCosts defCV (defDP {_desiredPropertiesRepairWithin = Minutes}) ^. storageServers . servers `shouldBe` 0.1
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 10 PB, _desiredPropertiesSharedInfrastracture = False, _desiredPropertiesRepairWithin = Minutes}) ^. storageServers . servers `shouldBe` 94
    it "calculates number of racks" $ do
      rnd 2 (calculateCosts defCV defDP ^. storageServers . racks) `shouldBe` 0.01
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 8 PB, _desiredPropertiesSharedInfrastracture = False, _desiredPropertiesRepairWithin = Minutes}) ^. storageServers . racks `shouldBe` 12.5
    it "calculates number of replacements" $ do
      calculateCosts defCV defDP ^. storageServers . replacements `shouldBe` 2
      calculateCosts defCV (defDP {_desiredPropertiesLifetime = 30}) ^. storageServers . replacements `shouldBe` 6
      calculateCosts defCV (defDP {_desiredPropertiesLifetime = 5}) ^. storageServers . replacements `shouldBe` 1
      calculateCosts (defCV {_configVariablesLifetimeServer = 2}) defDP ^. storageServers . replacements `shouldBe` 5
      calculateCosts (defCV {_configVariablesLifetimeServer = 2.5}) defDP ^. storageServers . replacements `shouldBe` 4
    it "calculates servers costs" $ do
      calculateCosts defCV defDP ^. storageServers . serversCosts `shouldBe` 750
      calculateCosts defCV (defDP {_desiredPropertiesLifetime = 30}) ^. storageServers . serversCosts `shouldBe` 2250
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 42 TB}) ^. storageServers . serversCosts `shouldBe` 4000
      calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. storageServers . serversCosts `shouldBe` 10000
    it "calculates racks costs" $ do
      rnd 2 (calculateCosts defCV defDP ^. storageServers . racksCosts) `shouldBe` 250
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 30}) ^. storageServers . racksCosts) `shouldBe` 750
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 42 TB}) ^. storageServers . racksCosts) `shouldBe` 1333.33
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. storageServers . racksCosts) `shouldBe` 3333.33
    it "calculates power costs (for lifetime)" $ do
      rnd 2 (calculateCosts defCV defDP ^. storageServers . powerCosts) `shouldBe` 262.8
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 30}) ^. storageServers . powerCosts) `shouldBe` 788.4
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 42 TB}) ^. storageServers . powerCosts) `shouldBe` 1401.6
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. storageServers . powerCosts) `shouldBe` 3504
  describe "Networking" $ do
    it "calculates if firewall should be used" $ do
      calculateCosts defCV defDP ^. networking . firewalls `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = ClosedData}) ^. networking . firewalls `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewalls `shouldBe` True
    it "calculates backup bandwidth in TB" $ do
      calculateCosts defCV defDP ^. networking . bandwidthBackup `shouldBe` 2
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0.3}) ^. networking . bandwidthBackup `shouldBe` 6
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. networking . bandwidthBackup `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 40 TB}) ^. networking . bandwidthBackup `shouldBe` 8
      calculateCosts defCV (defDP {_desiredPropertiesTapeBackup = False}) ^. networking . bandwidthBackup `shouldBe` 1
    it "calculates bandwidth in TB per day" $ do
      calculateCosts defCV defDP ^. networking . bandwidthOut `shouldBe` 1
      calculateCosts defCV (defDP {_desiredPropertiesDailyReadVolume = 0.3}) ^. networking . bandwidthOut `shouldBe` 3
      calculateCosts defCV (defDP {_desiredPropertiesDailyReadVolume = 0}) ^. networking . bandwidthOut `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 40 TB}) ^. networking . bandwidthOut `shouldBe` 4
    it "calculates costs of network ports" $ do
      calculateCosts defCV defDP ^. networking . networkPorts `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . networkPorts `shouldBe` 2520
      calculateCosts (defCV {_configVariablesNetworkPortRent = 7}) (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . networkPorts `shouldBe` 1800
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive, _desiredPropertiesLifetime = 15}) ^. networking . networkPorts `shouldBe` 3780
    it "calculates costs of bandwidth" $ do
      rnd 2 (calculateCosts defCV defDP ^. networking . bandwidthCosts) `shouldBe` 5475
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 15}) ^. networking . bandwidthCosts) `shouldBe` 8212.5
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesDailyReadVolume = 0.3}) ^. networking . bandwidthCosts) `shouldBe` 9125
      rnd 2 (calculateCosts (defCV {_configVariablesNetworkCost = 0.3}) defDP ^. networking . bandwidthCosts) `shouldBe` 3285
    it "calculates costs of firewall" $ do
      calculateCosts defCV defDP ^. networking . firewallCosts `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallCosts `shouldBe` 2000
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive, _desiredPropertiesDailyChanges = 0}) ^. networking . firewallCosts `shouldBe` 1000
      calculateCosts (defCV {_configVariablesFirewallCost = 500}) (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallCosts `shouldBe` 1000
    it "calculates costs of firewall setup" $ do
      calculateCosts defCV defDP ^. networking . firewallSetup `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallSetup `shouldBe` 280
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive, _desiredPropertiesDailyChanges = 0}) ^. networking . firewallSetup `shouldBe` 210
      calculateCosts (defCV {_configVariablesCostMHR = 100}) (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallSetup `shouldBe` 400
    it "calculates costs of firewall maintenance" $ do
      calculateCosts defCV defDP ^. networking . firewallMaintenance `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallMaintenance `shouldBe` 840
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive, _desiredPropertiesLifetime = 15}) ^. networking . firewallMaintenance `shouldBe` 1260
      calculateCosts (defCV {_configVariablesCostMHR = 100}) (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallMaintenance `shouldBe` 1200
      calculateCosts (defCV {_configVariablesFirewallMaintenance = 0.5}) (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. networking . firewallMaintenance `shouldBe` 4200
  describe "TapeBackup" $ do
    it "calculates restore time in hours" $ do
      rnd 2 (calculateCosts defCV defDP ^. tapeBackup . restoreTime) `shouldBe` 23.15
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 40 TB}) ^. tapeBackup . restoreTime) `shouldBe` 92.59
      rnd 2 (calculateCosts (defCV {_configVariablesTapeSpeed = 500}) defDP ^. tapeBackup . restoreTime) `shouldBe` 20
      rnd 2 (calculateCosts (defCV {_configVariablesTapeSpeed = 330}) defDP ^. tapeBackup . restoreTime) `shouldBe` 30.3
    it "calculates acceptable repair time in hours" $ do
      calculateCosts defCV defDP ^. tapeBackup . acceptableRepairTime `shouldBe` 96
      calculateCosts defCV (defDP {_desiredPropertiesRepairWithin = Hours}) ^. tapeBackup . acceptableRepairTime `shouldBe` 4
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesRepairWithin = Minutes}) ^. tapeBackup . acceptableRepairTime) `shouldBe` 0.07
      calculateCosts defCV (defDP {_desiredPropertiesRepairWithin = Weeks}) ^. tapeBackup . acceptableRepairTime `shouldBe` 672
    it "calculates if can share tapes" $ do
      calculateCosts defCV defDP ^. tapeBackup . sharedTape `shouldBe` True
      calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. tapeBackup . sharedTape `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. tapeBackup . sharedTape `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. tapeBackup . sharedTape `shouldBe` False
    it "calculates volume of daily backup in TB" $ do
      calculateCosts defCV defDP ^. tapeBackup . dailyBackup `shouldBe` 1
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. tapeBackup . dailyBackup `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0.3}) ^. tapeBackup . dailyBackup `shouldBe` 3
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. tapeBackup . dailyBackup `shouldBe` 5
    it "calculates backup time in hours" $ do
      rnd 2 (calculateCosts defCV defDP ^. tapeBackup . backupTime) `shouldBe` 4.63
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 40 TB}) ^. tapeBackup . backupTime) `shouldBe` 18.52
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesContentType = FewLargeFiles}) ^. tapeBackup . backupTime) `shouldBe` 2.31
      rnd 2 (calculateCosts (defCV {_configVariablesTapeSpeed = 330}) defDP ^. tapeBackup . backupTime) `shouldBe` 6.06
    it "calculates number of tapes" $ do
      rnd 2 (calculateCosts defCV defDP ^. tapeBackup . tapes) `shouldBe` 1.07
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 7 TB}) ^. tapeBackup . tapes) `shouldBe` 0.75
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 40 TB}) ^. tapeBackup . tapes) `shouldBe` 16
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. tapeBackup . tapes) `shouldBe` 16
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesBackupHistory = Days}) ^. tapeBackup . tapes) `shouldBe` 0.33
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesBackupHistory = Weeks}) ^. tapeBackup . tapes) `shouldBe` 0.67
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesBackupHistory = Years}) ^. tapeBackup . tapes) `shouldBe` 1.73
      rnd 2 (calculateCosts (defCV {_configVariablesTapeCapacity = 25}) defDP ^. tapeBackup . tapes) `shouldBe` 0.64
    it "calculates number of tape drives" $ do
      rnd 2 (calculateCosts defCV defDP ^. tapeBackup . drives) `shouldBe` 0.19
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 7 TB}) ^. tapeBackup . drives) `shouldBe` 0.14
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 40 TB}) ^. tapeBackup . drives) `shouldBe` 1
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. tapeBackup . drives) `shouldBe` 1
    it "calculates if tape robot should be used" $ do
      calculateCosts defCV defDP ^. tapeBackup . tapeRobot `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 500 TB}) ^. tapeBackup . tapeRobot `shouldBe` True
      calculateCosts (defCV {_configVariablesTapeCapacity = 0.5}) defDP ^. tapeBackup . tapeRobot `shouldBe` True
    it "calculates hours of tape operator work" $ do
      rnd 2 (calculateCosts defCV defDP ^. tapeBackup . tapeOperator) `shouldBe` 19.29
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. tapeBackup . tapeOperator) `shouldBe` 100
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. tapeBackup . tapeOperator) `shouldBe` 100
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 15}) ^. tapeBackup . tapeOperator) `shouldBe` 28.94
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. tapeBackup . tapeOperator) `shouldBe` 0
    it "calculates if tape index should be used" $ do
      calculateCosts defCV defDP ^. tapeBackup . needTapeIndex `shouldBe` False
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. tapeBackup . needTapeIndex `shouldBe` True
    it "calculates total tapes costs" $ do
      calculateCosts defCV defDP ^. tapeBackup . totalTapeCosts `shouldBe` 160
      calculateCosts defCV (defDP {_desiredPropertiesTapeBackup = False}) ^. tapeBackup . totalTapeCosts `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesDailyChanges = 0}) ^. tapeBackup . totalTapeCosts `shouldBe` 0
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 30 TB}) ^. tapeBackup . totalTapeCosts `shouldBe` 2400
      calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. tapeBackup . totalTapeCosts `shouldBe` 2400
      calculateCosts (defCV {_configVariablesTapeCost = 120}) defDP ^. tapeBackup . totalTapeCosts `shouldBe` 128
    it "calculates total tape drives costs" $ do
      rnd 0 (calculateCosts defCV defDP ^. tapeBackup . totalDriveCosts) `shouldBe` 579
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. tapeBackup . totalDriveCosts) `shouldBe` 60000
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. tapeBackup . totalDriveCosts) `shouldBe` 3000
      rnd 0 (calculateCosts (defCV {_configVariablesTapeDriveCost = 1200}) defDP ^. tapeBackup . totalDriveCosts) `shouldBe` 231
    it "calculates total tape robot costs" $ do
      rnd 0 (calculateCosts defCV defDP ^. tapeBackup . totalRobotCosts) `shouldBe` 0
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. tapeBackup . totalRobotCosts) `shouldBe` 50000
      rnd 0 (calculateCosts (defCV {_configVariablesTapeRobotCost = 10000}) (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. tapeBackup . totalRobotCosts) `shouldBe` 10000
    it "calculates total tape operator costs" $ do
      rnd 0 (calculateCosts defCV defDP ^. tapeBackup . operatorCosts) `shouldBe` 1350
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 1 PB}) ^. tapeBackup . operatorCosts) `shouldBe` 7000
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. tapeBackup . operatorCosts) `shouldBe` 7000
      rnd 0 (calculateCosts (defCV {_configVariablesCostMHR = 50}) defDP ^. tapeBackup . operatorCosts) `shouldBe` 965
  describe "Setup" $ do
    it "calculates operator hours" $ do
      rnd 2 (calculateCosts defCV defDP ^. setup . operator) `shouldBe` 9.46
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. setup . operator) `shouldBe` 16.17
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. setup . operator) `shouldBe` 15.97
    it "calculates operator costs" $ do
      rnd 0 (calculateCosts defCV defDP ^. setup . operatorCosts) `shouldBe` 663
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. setup . operatorCosts) `shouldBe` 1132
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. setup . operatorCosts) `shouldBe` 1118
      rnd 0 (calculateCosts (defCV {_configVariablesCostMHR = 100}) defDP ^. setup . operatorCosts) `shouldBe` 946
  describe "IncidentResponse" $ do
    it "calculates frequency per year" $ do
      rnd 2 (calculateCosts defCV defDP ^. incidentResponse . frequency) `shouldBe` 1.09
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. incidentResponse . frequency) `shouldBe` 6.82
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. incidentResponse . frequency) `shouldBe` 1.67
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. incidentResponse . frequency) `shouldBe` 1.67
    it "calculates operator costs" $ do
      rnd 0 (calculateCosts defCV defDP ^. incidentResponse . operatorCosts) `shouldBe` 381
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 50 TB}) ^. incidentResponse . operatorCosts) `shouldBe` 2389
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesSharedInfrastracture = False}) ^. incidentResponse . operatorCosts) `shouldBe` 583
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesSecurityLevel = PrivacySensitive}) ^. incidentResponse . operatorCosts) `shouldBe` 875
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesRepairTimes = D5H10}) ^. incidentResponse . operatorCosts) `shouldBe` 381
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesRepairTimes = D7H24}) ^. incidentResponse . operatorCosts) `shouldBe` 1144
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 15}) ^. incidentResponse . operatorCosts) `shouldBe` 572
      rnd 0 (calculateCosts (defCV {_configVariablesCostMHR = 100}) defDP ^. incidentResponse . operatorCosts) `shouldBe` 545
  describe "UninterruptiblePowerSupplies" $
    it "calculates total costs" $ do
      calculateCosts defCV defDP ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 630
      calculateCosts defCV (defDP {_desiredPropertiesLifetime = 15}) ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 945
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 30 TB}) ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 1830
      calculateCosts defCV (defDP {_desiredPropertiesUnavailability = Hour}) ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 1890
      calculateCosts (defCV {_configVariablesHDDPower = 0.5}) defDP ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 330
      calculateCosts (defCV {_configVariablesServerPower = 250}) defDP ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 675
      calculateCosts (defCV {_configVariablesUPS = 0.2}) defDP ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 1260
  describe "StorageCosts" $ do
    it "calculates total costs" $ do
      rnd 2 (calculateCosts defCV defDP ^. total) `shouldBe` 13657.58
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 30 TB}) ^. total) `shouldBe` 45690.13
      rnd 2 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 15}) ^. total) `shouldBe` 19785.76
    it "calculates costs per year" $ do
      rnd 0 (calculateCosts defCV defDP ^. perYear) `shouldBe` 137
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 30 TB}) ^. perYear) `shouldBe` 152
      rnd 0 (calculateCosts defCV (defDP {_desiredPropertiesLifetime = 15}) ^. perYear) `shouldBe` 132
