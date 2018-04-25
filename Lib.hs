module Lib where

import Data.Maybe (fromJust)

-- Eq, Ord, Enum, Show, Bounded

data Week = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday


instance Eq Week where
  Monday    == Monday = True
  Tuesday   == Tuesday = True
  Wednesday == Wednesday = True
  Thursday  == Thursday = True
  Friday    == Friday = True
  Saturday  == Saturday = True
  Sunday    == Sunday  = True
  _         == _ = False

instance Show Week where
  show Monday = "Monday"
  show Tuesday = "Tuesday"
  show Wednesday = "Wednesday"
  show Thursday = "Thursday"
  show Friday = "Friday"
  show Saturday = "Saturday"
  show Sunday = "Sunday"

wnums = zip [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday] [1..]

swap (x,y) = (y,x)

-- instance Enum Week where
--   fromEnum = fromJust . flip lookup wnums
--   toEnum = fromJust . flip lookup (map swap wnums)

instance Enum Week where
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6
  fromEnum Sunday = 7

  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum 7 = Sunday
  -- what magic is done with "deriving Enum" such that [Monday ..] does not fail?
  -- it doesn't depend on a Bounded instance
  toEnum _ = error "toEnum :: Week called with invalid input"

  -- this assumes you have a Bounded instance!
  -- see https://hackage.haskell.org/package/base-4.11.1.0/docs/Prelude.html#t:Enum
  enumFrom w = [w .. maxBound]

instance Bounded Week where
  minBound = Monday
  maxBound = Sunday

instance Ord Week where
  compare a b
    | a == b = EQ
    | otherwise = ordhelp a b

ordhelp a b = compare (numval a) (numval b)
  where numval = fromJust . flip lookup wnums

-- ordhelp Monday Tuesday = LT
-- ordhelp Tuesday Wednesday = LT
-- ordhelp Wednesday Thursday = LT
-- ordhelp Thursday Friday = LT
-- ordhelp Friday Saturday = LT
-- ordhelp Saturday Sunday = LT
-- ordhelp _ _ = GT
