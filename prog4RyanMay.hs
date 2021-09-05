-------------------------------------------------------------
--------------------Programming Task 4-----------------------
-------------------------------------------------------------
--  Name:  RYAN MAY                                        --
--  Email: rmay03@syr.edu                                  --
-------------------------------------------------------------
import RPGdefs

-------------------------------------------------------------
-- PROBLEM #1
-- Purpose: 
--    Defintions of various mage and warrior characters

-- Definitions:
-- Mages (problems 1a - 1d)
mage1 = Mage 5 Health Potion
mage2 = Mage 2 Wealth Gold
mage3 = Mage 4 Wealth (Weapon 6)
mage4 = Mage 7 None (Shield 7)

-- Warriors (problems 1e - 1h)
fighter1 = Warrior Archer []
fighter2 = Warrior Brute [Gold, Gold, Amulet, (Weapon 6), (Weapon 2)]
fighter3 = Warrior Archer [Potion, Potion, Potion, Amulet, Amulet, Gold, (Shield 3)]
fighter4 = Warrior Swordster [(Shield 4), (Shield 3), (Weapon 2), Potion,Amulet,Amulet,Gold,Gold,Gold]


-------------------------------------------------------------
-- PROBLEM #2
-- Purpose: 
--    cost item
--       This function takes an item and returns the cost of the item passed in.

-- Defintion:
cost :: Item -> Integer
cost (Weapon n) = 2*n + 1
cost (Shield n) = 2*n
cost Potion = 3
cost _ = 1

-- Tests:
--   Tests include one of each type of item. The defensive/offensive value for the shield and weapon
--   (respectively) test is the same to show they differ in cost.

t2a = cost Gold            -- Should return 1 (Cost of Gold is 1)
t2b = cost Potion          -- Should return 3 (Cost of Potion is 1)
t2c = cost (Shield 3)      -- Should return 6 (Cost of shield is 2*3=6)
t2d = cost (Weapon 3)      -- Should return 7 (Cost of Weapon is 2*3+1=7)
t2e = cost Amulet          -- Should return 1 (Cost of Amulet)

-------------------------------------------------------------
-- PROBLEM #3
-- Purpose: 
--    hasPotion hero
--       This function takes a Hero as a parameter and returns a Bool that indicates
--       whether the Hero posseses a Potion or not.

-- Defintion:
hasPotion :: Hero -> Bool
hasPotion (Mage _ _ _) = True
hasPotion (Mage _ _ Potion) = True
hasPotion (Warrior _ (Potion:xs)) =  True
hasPotion (Warrior s (x:xs)) = hasPotion (Warrior s xs)
hasPotion hero = False

-- Tests:
--   Tests include a Mage with a Potion and a mage without a potion.  Tests also include various warriors passed in,
--   one with empty list of items, one with list of items with no potion, one with list of items with 1 potion, and
--   finally one with a list of items with multiple potions.

t3a = hasPotion (Mage 0 None Potion)                            -- Should return True (Mage only item is potion)
t3b = hasPotion (Mage 2 Wealth (Weapon 3))                      -- Should return False (Mage only item is weapon, diff ability/off power)
t3c = hasPotion (Warrior Brute [])                              -- Should return False (Warrior with empty item list)
t3d = hasPotion (Warrior Archer [Gold,Weapon 3,Shield 6])       -- Should return False (Warrior with list with no Potion)
t3e = hasPotion (Warrior Swordster [Gold, Potion, Shield 6])    -- Should Return True (Warrior with a Potion in list of items)
t3f = hasPotion (Warrior Archer [Gold,Potion,Potion,Potion])    -- Should return True (Warrior with multiple potions in list of items)

-------------------------------------------------------------
-- PROBLEM #4
-- Purpose:
--    canBuy hero item
--       This function takes a Hero and an Item as parameters and returns a Bool indicating
--       whether the hero passed in is able to purchase/afford the item passed in.

-- Defintion:
canBuy :: Hero -> Item -> Bool
canBuy (Mage _ Wealth Gold) _ = True
canBuy (Mage _ _ Gold) item = ((cost item) == 1)
canBuy (Warrior _ xs) item = amt >= (cost item)
    where 
      amt = sum [ 1 | Gold <-xs]
canBuy _ _ = False

-- Tests:
--   Tests include a mage with gold and the wealth ability, a mage w/out wealth ability but posseses gold coin, and a mage
--   that does not posses a gold coin at all.  Tests also include sever warriors, one with an empty item list, and several
--   other Warriors with various amounts of gold. 

t4a = canBuy mage2 (Shield 100)                     -- Should return True (Mage 2 has Wealth ability and can buy anything
t4b = canBuy (Mage 2 Health Gold) Amulet            -- Should return True (Mage possessing one Gold as their item)
t4c = canBuy (Mage 2 Wealth Amulet) (Shield 2)      -- Should return False (Mage possessing Wealth but no Gold)
t4d = canBuy fighter1 Gold                          -- Should return False (Warrior with empty item list)
t4e = canBuy fighter2 (Shield 1)                    -- Should return True (Warrior with 2 gold can afford item of cost 2)
t4f = canBuy fighter2 (Shield 2)                    -- Should Return False (Same items as t4e but Shield is now too expensive)
t4g = canBuy fighter4 (Weapon 1)                    -- Should return True (Warrior with 3 gold can afford item of cost 3)

-------------------------------------------------------------
-- PROBLEM #5
-- Purpose: 
--    boostShields n hero
--       This function takes an Integer, n, and a Hero passed in and boosts
--       any Shields the hero possesses by the value n.

-- Defintion:
boostShields :: Integer -> Hero -> Hero
boostShields n (Mage op ab (Shield x)) = (Mage op ab (Shield (x+n)))
boostShields n (Warrior st items) = Warrior st newList
    where
      addShield :: Item -> Item
      addShield (Shield x) = Shield (x+n)
      addShield itm = itm
      newList = [ addShield y | y <-items]
boostShields _ hero = hero

-- Tests:
--   Tests include a mage that possesses a shield and a mage that does not posesses a shield. Tests also include
--   a Warrior with an empty item list, a Warrior with a non empty list but no shields, and Warriors with various amounts
--   of shields in their item list.

t5a = boostShields 6 (Mage 4 None (Shield 10))           -- Should return Mage 4 None (Shield 6)  (Mage w/ Shield of 10 boosted by 6)
t5b = boostShields 2 (Mage 3 Health Gold)                -- Should return Mage 3 Health Gold  (No shield to boost)
t5c = boostShields 3 fighter1                            -- Should return Warrior Archer [] (Warrior with empty list, returns original Hero)
t5d = boostShields 4 fighter2                            -- Should return Warrior Brute [Gold,Gold,Amulet,Weapon 6,Weapon 2] (Warrior w no shield in item list)
t5e = boostShields 5 fighter3                            -- Should return Warrior Archer [Potion,Potion,Potion,Amulet,Amulet,Gold,Shield 8] (Warrior w one Shield)
fighterShield = (Warrior Brute [(Shield 1),(Shield 2),(Shield 3)])
t5g = boostShields 10 fighterShield                      -- Should return Warrior Brute [Shield 11,Shield 12,Shield 13] (Multiple Shields boosted)

-------------------------------------------------------------
-- PROBLEM #6
-- Purpose: 
--    heroPower hero
--       This function takes a hero in and returns the total offensive power of the hero, which
--       is calculated by the sum of all heros weopon powers, plus their base offensive power.


-- Defintion:
heroPower :: Hero -> Integer
heroPower (Mage base _ (Weapon wpn)) = base + wpn
heroPower (Mage base _ _) = base
heroPower (Warrior st list) = (style st) + wpn
    where
      style :: Style -> Integer
      style Archer = 2
      style Swordster = 3
      style Brute = 5
      wpn = sum([ x | (Weapon x) <- list])

-- Tests:
--   Tests include a mage that possesses a weapon and a mage that does not posesses a weapon. Tests also include
--   a Warrior with an empty item list, a Warrior with a non empty list but no weapons, and Warriors with various amounts
--   of weapons in their item list.

t6a = heroPower (Mage 4 Health (Weapon 10))                -- Should return 14 (Mage w/ Shield of 10 boosted by 6)
t6b = heroPower (Mage 3 Wealth Gold)                       -- Should return 3  (Mage with no shield to boost)
t6c = heroPower (Warrior Brute [])                         -- Should return 5  (Warrior with empty item list)
t6d = heroPower (Warrior Archer [Amulet,Gold,Shield 2])    -- Should return 2  (Warrior w no weapon in item list)
t6e = heroPower (Warrior Swordster [Weapon 4])             -- Should return 7  (Warrior w one Weapon)
t6g = heroPower (Warrior Brute [Weapon 3, Weapon 5])       -- Should return 13 (Warrior with multiple weapons)

-------------------------------------------------------------
-- PROBLEM #7
-- Purpose: 
--    canSurvive hero force
--       This function takes a Hero and an Integer, force, passed in and returns a boolean indicating
--       whether the hero can survive the amount of force passed in, with regards to their total shield 
--       defense and health capabilites(for mages).


-- Defintion:
canSurvive :: Hero -> Integer -> Bool
canSurvive (Mage _ Health _) _ = True
canSurvive (Mage _ _ (Shield def)) force = force <= def
canSurvive (Warrior _ lst) force = force <= def
    where
      shieldAdd :: Item -> Integer
      shieldAdd (Shield x) = x
      shieldAdd itm = 0
      def = sum ( [ shieldAdd itm | itm <- lst] )
canSurvive _ _ = False

-- Tests:
--   Tests include a mage that possesses a Shield and a mage that does not posesses a Shield, and a mage that has the Health
--   ability and therefore can survive any attack. Tests also include a Warrior with an empty item list, a Warrior with a non empty 
--   list but no Shields, and Warriors with various amounts of Shields in their item list.

t7a = canSurvive (Mage 4 Health (Weapon 10)) 300            -- Should return True (Mage w/ Health ability can survive any attack)
t7b = canSurvive (Mage 3 None (Shield 5)) 5                 -- Should return True (Mage with Shield of 5 can survive 5 force)
t7c = canSurvive (Mage 3 Wealth (Weapon 4)) 5               -- Should return False (Mage has no defense)
t7d = canSurvive (Warrior Brute []) 3                       -- Should return False (Warrior with empty item list)
t7e = canSurvive (Warrior Archer [Amulet,Weapon 2]) 4       -- Should return False (Warrior w no shield in item list)
t7f = canSurvive (Warrior Swordster [Shield 4]) 3           -- Should return True (Warrior w one Shield)
t7g = canSurvive (Warrior Brute [Shield 3, Shield 5]) 8     -- Should return True (Warrior with multiple Shields)
t7h = canSurvive (Warrior Brute [Shield 3, Shield 5]) 9     -- Should return False (Same as t7g but more force)

-------------------------------------------------------------
-- PROBLEM #8
-- Purpose: 
--    bestShield hero
--       This function takes hero passed in and returns the value of the strongest shield the hero
--       possesses. If the hero does not have a Shield the function returns Nothing.


-- Defintion:
bestShield :: Hero -> Maybe Integer
bestShield (Mage _ _ (Shield x)) = Just x
bestShield (Warrior _ lst)
    | length lst == 0 = Nothing
    | x > 0     = Just x
    where
      shield :: Item -> Integer
      shield (Shield s) = s
      shield itm = 0
      x = maximum ( [ shield r | r <- lst])
bestShield hero = Nothing

-- Tests:
--   Tests include a mage with a shield, and a mage without a shield.  Tests also include a Warrior with an empty item list,
--   a Warrior with only one shield, and a warrior with multiple shields. Also one Warrior test with multiple shields but in 
--   a different order in the list.

t8a = bestShield (Mage 4 Health (Weapon 10))                     -- Should return Nothing (Mage with no Shield)
t8b = bestShield (Mage 3 None (Shield 5))                        -- Should return Just 5 (Mage w/ 1 Shield, can only possess one shield)
t8c = bestShield (Warrior Brute [])                              -- Should return Nothing (Warrior with empty item list)
t8d = bestShield (Warrior Archer [Amulet,Weapon 2])              -- Should return Nothing (Warrior w no shield in item list)
t8e = bestShield (Warrior Swordster [Shield 4])                  -- Should return Just 4 (Warrior w one Shield)
t8f = bestShield (Warrior Brute [Shield 3,Shield 5,Shield 8])    -- Should return Just 8 (Warrior with multiple Shields)
t8g = bestShield (Warrior Brute [Shield 3,Shield 8,Shield 5])    -- Should return Just 8 (Same as t7g but shields in different order)

-------------------------------------------------------------
-- PROBLEM #9
-- Purpose: 
--    removeGold amt items
--       This function takes an Integer and a list of items passed in, and uses the Integer
--       amt passed in to subtract that amount of gold from the list of items. If the amt is
--       more than the amount of Gold in the list of items then all of the gold will be removed.
--       The resulting list is returned.


-- Defintion:
removeGold :: Integer -> [Item] -> [Item]
removeGold amt lst
    | length lst == 0 = []
    | amt <= 0 = lst
removeGold amt lst@(Gold:xs)= removeGold (amt-1) xs
removeGold amt lst@(x:xs) = x: removeGold amt xs

-- Tests:
--   Tests include passing in a negative amount to remove, passing in 0 for an amount to remove, Removing a single
--   element from the list, removing from a list when the amount to remove is more than the amount of gold the list has,
--   and a test with the empty list as well.

t9a = removeGold (-4) [Gold,Gold,Amulet]                         -- Should return [Gold,Gold,Amulet] (Amount to remove is a neg number)
t9b = removeGold 0 [Gold,Gold,Amulet]                            -- Should return [Gold,Gold,Amulet] (Amount to remove is zero)
t9c = removeGold 1 [Gold,Gold,Amulet]                            -- Should return [Gold,Amulet] (Amount to remove is less than the list has)
t9d = removeGold 4 [Gold,Shield 4,Gold]                          -- Should return [Shield 4] (Amount to remove is more than the list has)
t9e = removeGold 4 [Gold,Gold,Amulet,Gold,Potion,Gold,Gold]      -- Should return [Amulet,Potion,Gold] (Gold showing up in dif. areas of the list)
t9f = removeGold 6 []                                            -- Should return [] (Empty list case)



-------------------------------------------------------------
-- PROBLEM #10
-- Purpose: 
--    buyItem hero itm 
--       This function takes a hero and an item passed in.  If the hero passed in can afford
--       to buy the item passed in, then the item is added to the hero, and the correct amount
--       of gold is deducted from the hero(Price in gold of item passed in)

-- Defintion:
buyItem :: Hero -> Item -> Hero
buyItem hero itm
    | canBuy hero itm = newHero hero
    | otherwise = hero
    where
      newHero :: Hero -> Hero
      newHero (Mage x y _) = Mage x y itm
      newHero (Warrior x ls) = Warrior x (itm : (removeGold (cost itm) ls))

-- Tests:
--   Tests include passing in a Mage that can afford to buy an item, and a Mage that cannot afford to buy the item passed in.
--   Tests also include a mage with the Wealth ability and a Gold item, in which case this mage is able to afford any item (except after
--   the purchase is completed).  Tests also include a Warrior with an empty item list, a Warrior with sufficient amount of gold to buy
--   the item passed in, and a Warrior with an insufficient amount of gold to buy the item passed in.

t10a = buyItem mage1 Amulet              -- Should return Mage 5 Health Potion (mage1 can't afford item, original hero returned)
t10b = buyItem mage2 (Shield 20)         -- Should return Mage 2 Wealth (Shield 20) (mage2 can afford any item, hero returned with Shield as item)
t10c = buyItem (Mage 2 None Gold) Amulet -- Should return Mage 2 None Amulet (Mage that can afford an Amulet, Amulet replaces gold)
t10d = buyItem fighter1 Gold             -- Should return Warrior Archer [] (Warrior with an empty list, ie. cant afford any item)
t10e = buyItem fighter4 (Weapon 1)       -- Should return Warrior Swordster [Weapon 1,Shield 4,Shield 3,Weapon 2,Potion,Amulet,Amulet] (Weapon added,gold rmvd)
t10f = buyItem fighter3 (Weapon 1)       -- Should return Warrior Archer [Potion,Potion,Potion,Amulet,Amulet,Gold,Shield 3] (Same item as t10e,but cant afford)