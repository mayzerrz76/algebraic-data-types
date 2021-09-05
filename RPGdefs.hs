module RPGdefs where

----------------------------------------------------------------------
--  Algebraic Datatypes for Task #4 (CIS 252: Spring 2021)
----------------------------------------------------------------------

-- items that can be held by heroes
data Item =  Amulet | Gold | Potion | Shield Integer | Weapon Integer
             deriving (Show)

-- abilities for mages
data Ability = Wealth | Health | None
             deriving (Show)

-- styles for warriors
data Style = Archer | Swordster | Brute
             deriving (Show)

-- the heroes themselves
data Hero = Mage Integer Ability Item
          | Warrior Style [Item]
            deriving (Show)

