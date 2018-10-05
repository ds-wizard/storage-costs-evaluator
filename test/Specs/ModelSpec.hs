module Specs.ModelSpec (spec) where

import Control.Lens
import Test.Hspec

import Specs.Data.Inputs

import Model.Calculation
import Model.CalculationEntities
import Model.Common
import Model.Inputs

spec :: Spec
spec = do
  describe "StorageDrives" $ do
    it "calculates volume in TB" $ do
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 7 TB}) ^. storageDrives . usableVolume `shouldBe` 7
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 10000 GB}) ^. storageDrives . usableVolume `shouldBe` 10
      calculateCosts defCV (defDP {_desiredPropertiesVolume = Volume 9 PB}) ^. storageDrives . usableVolume `shouldBe` 9000
    it "calculates daily backup in TB" $ do
      calculateCosts defCV defDP ^. storageDrives . backup `shouldBe` 0
    it "calculates needed number of HDDs (redundancy)" $ do
      calculateCosts defCV defDP ^. storageDrives . redundancy `shouldBe` 2
    it "calculates raw volume" $ do
      calculateCosts defCV defDP ^. storageDrives . rawVolume `shouldBe` 20
    it "calculates needed space" $ do
      calculateCosts defCV defDP ^. storageDrives . spaceNeeded `shouldBe` 25
    it "calculates number of replacements" $ do
      calculateCosts defCV defDP ^. storageDrives . replacements `shouldBe` 10/3
    it "calculates total drive costs" $ do
      calculateCosts defCV defDP ^. storageDrives . totalDriveCosts `shouldBe` 2500
    it "calculates power costs (for lifetime)" $ do
      round (calculateCosts defCV defDP ^. storageDrives . powerCosts) `shouldBe` 6570
  describe "StorageServers" $ do
    it "calculates number of server drives" $ do
      calculateCosts defCV defDP ^. storageServers . drives `shouldBe` 3
    it "calculates if can be shared" $ do
      calculateCosts defCV defDP ^. storageServers . shared `shouldBe` True
    it "calculates number of servers" $ do
      calculateCosts defCV defDP ^. storageServers . servers `shouldBe` 7
    it "calculates number of racks" $ do
      calculateCosts defCV defDP ^. storageServers . racks `shouldBe` 7
    it "calculates number of replacements" $ do
      calculateCosts defCV defDP ^. storageServers . replacements `shouldBe` 2
    it "calculates servers costs" $ do
      calculateCosts defCV defDP ^. storageServers . serversCosts `shouldBe` 2
    it "calculates racks costs" $ do
      calculateCosts defCV defDP ^. storageServers . racksCosts `shouldBe` 2
    it "calculates power costs (for lifetime)" $ do
      calculateCosts defCV defDP ^. storageServers . powerCosts `shouldBe` 2
  describe "Networking" $ do
    it "calculates if firewall should be used" $ do
      calculateCosts defCV defDP ^. networking . firewalls `shouldBe` True
    it "calculates backup bandwidth in TB" $ do
      calculateCosts defCV defDP ^. networking . bandwidthBackup `shouldBe` 2
    it "calculates bandwidth in TB per day" $ do
      calculateCosts defCV defDP ^. networking . bandwidthOut `shouldBe` 2
    it "calculates costs of network ports" $ do
      calculateCosts defCV defDP ^. networking . networkPorts `shouldBe` 2
    it "calculates costs of bandwidth" $ do
      calculateCosts defCV defDP ^. networking . bandwidthCosts `shouldBe` 2
    it "calculates costs of firewall" $ do
      calculateCosts defCV defDP ^. networking . firewallCosts `shouldBe` 2
    it "calculates costs of firewall setup" $ do
      calculateCosts defCV defDP ^. networking . firewallSetup `shouldBe` 2
    it "calculates costs of firewall maintenance" $ do
      calculateCosts defCV defDP ^. networking . firewallMaintenance `shouldBe` 2
  describe "TapeBackup" $ do
    it "calculates restore time in hours" $ do
      calculateCosts defCV defDP ^. tapeBackup . restoreTime `shouldBe` 2
    it "calculates acceptable repair time in hours" $ do
      calculateCosts defCV defDP ^. tapeBackup . acceptableRepairTime `shouldBe` 2
    it "calculates if can share tapes" $ do
      calculateCosts defCV defDP ^. tapeBackup . sharedTape `shouldBe` True
    it "calculates volume of daily backup in TB" $ do
      calculateCosts defCV defDP ^. tapeBackup . dailyBackup `shouldBe` 2
    it "calculates backup time in hours" $ do
      calculateCosts defCV defDP ^. tapeBackup . backupTime `shouldBe` 2
    it "calculates number of tapes" $ do
      calculateCosts defCV defDP ^. tapeBackup . tapes `shouldBe` 2
    it "calculates number of tape drives" $ do
      calculateCosts defCV defDP ^. tapeBackup . drives `shouldBe` 2
    it "calculates if tape robot should be used" $ do
      calculateCosts defCV defDP ^. tapeBackup . tapeRobot `shouldBe` True
    it "calculates hours of tape operator work" $ do
      calculateCosts defCV defDP ^. tapeBackup . tapeOperator `shouldBe` 2
    it "calculates if tape index should be used" $ do
      calculateCosts defCV defDP ^. tapeBackup . needTapeIndex `shouldBe` True
    it "calculates total tapes costs" $ do
      calculateCosts defCV defDP ^. tapeBackup . totalTapeCosts `shouldBe` 2
    it "calculates total tape drives costs" $ do
      calculateCosts defCV defDP ^. tapeBackup . totalDriveCosts `shouldBe` 2
    it "calculates total tape robot costs" $ do
      calculateCosts defCV defDP ^. tapeBackup . totalRobotCosts `shouldBe` 2
    it "calculates total tape operator costs" $ do
      calculateCosts defCV defDP ^. tapeBackup . operatorCosts `shouldBe` 2
  describe "Setup" $ do
    it "calculates operator hours" $ do
      calculateCosts defCV defDP ^. setup . operator `shouldBe` 2
    it "calculates operator costs" $ do
      calculateCosts defCV defDP ^. setup . operatorCosts `shouldBe` 2
  describe "IncidentResponse" $ do
    it "calculates frequency per year" $ do
      calculateCosts defCV defDP ^. incidentResponse . frequency `shouldBe` 2
    it "calculates operator costs" $ do
      calculateCosts defCV defDP ^. incidentResponse . operatorCosts `shouldBe` 2
  describe "UninterruptiblePowerSupplies" $ do
    it "calculates total costs" $ do
      calculateCosts defCV defDP ^. storageUninterruptiblePowerSupplies . totalCosts `shouldBe` 2
  describe "StorageCosts" $ do
    it "calculates total costs" $ do
      calculateCosts defCV defDP ^. total `shouldBe` 2
    it "calculates costs per year" $ do
      calculateCosts defCV defDP ^. perYear `shouldBe` 2
