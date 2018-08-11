module Model.Common where


type Percentage = Double -- between 0 and 1
type Years = Double

data VolumeUnit = GB | TB | PB
  deriving (Show, Read, Eq, Ord)

type Volume = (Double, VolumeUnit)

toTB :: Volume -> Double
toTB (x, GB) = x * 1000
toTB (x, TB) = x
toTB (x, PB) = x / 1000

whTokWHperYear :: Double -> Double
whTokWHperYear x = x * 365 * 24 / 100

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
                   deriving (Show, Read, Eq)

data YesNo = Yes | No deriving (Show, Read, Eq)

data Unavailability = Week | Day | Hour  deriving (Show, Read, Eq)

data SecurityLevel = OpenData
                   | ClosedData
                   | PrivacySensitive
                   deriving (Show, Read, Eq)

data AccessGroup = OnlyMe
                 | LocalCollaborators
                 | AuthorizedPeople
                 | OpenAccess
                 deriving (Show, Read, Eq)
