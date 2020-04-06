module Model.Common where

import Control.Lens


type Percentage = Double -- between 0 and 1
type Years = Double

data VolumeUnit = GB | TB | PB
  deriving (Show, Read, Eq, Ord)

data Volume = Volume
  { _volumeValue :: Double
  , _volumeUnit :: VolumeUnit
  } deriving (Show, Read, Eq)

makeFields ''Volume

toTB :: Volume -> Double
toTB (Volume x GB) = x / 1000
toTB (Volume x TB) = x
toTB (Volume x PB) = x * 1000

whTokWHperYear :: Double -> Double
whTokWHperYear x = x * 365 * 24 / 1000

data ContentType = ManySmallFiles
                 | FewLargeFiles
                 | Database
                 deriving (Show, Read, Eq)

data AccessType = OneFileOnRequest
                | RemoteFiles
                | DataBase
                | WebSite
                | CalculationServer
                | HighPerformance
                deriving (Show, Read, Eq)

data TimeHorizon = Seconds
                 | Minutes
                 | Hours
                 | Days
                 | Weeks
                 | Months
                 | Years
                 deriving (Show, Read, Eq)

toHours :: TimeHorizon -> Double
toHours Seconds = 1/3600
toHours Minutes = 1/60
toHours Hours = 1
toHours Days = 24
toHours Weeks = 7*24
toHours Months = 30*24
toHours Years = 365*24

data BusinessHours = D5H8
                   | D5H10
                   | D7H24
                   deriving (Show, Read, Eq, Ord)

data Unavailability = Hour | Day | Week deriving (Show, Read, Eq, Ord)

data SecurityLevel = OpenData
                   | ClosedData
                   | PrivacySensitive
                   deriving (Show, Read, Eq)

data AccessGroup = OnlyMe
                 | LocalCollaborators
                 | AuthorizedPeople
                 | OpenAccess
                 deriving (Show, Read, Eq)
