module Y2015.D21 where

import GHC.Natural (Natural)
import Utils ((-.-))
import Data.Text (Text)
import Data.List qualified as L
import Data.Maybe

data Entity =
  Entity
    { hitpoints :: Natural
    , damage :: Natural
    , armor :: Natural
    } deriving (Eq, Ord, Show)

boss :: Entity
boss = Entity 103 9 2

hero :: Entity
hero = Entity 100 0 0

winTurns :: (Entity, Entity) -> (Natural, Natural)
winTurns (e1,e2) =
  ( ceiling $ fromIntegral e1.hitpoints / fromIntegral (max 1 $ e2.damage -.- e1.armor)
  , ceiling $ fromIntegral e2.hitpoints / fromIntegral (max 1 $ e1.damage -.- e2.armor)
  )

data Item =
  Item
    { itemName :: Text
    , itemCost :: Natural
    , itemDamage :: Natural
    , itemArmor :: Natural
    } deriving (Eq, Ord, Show)

data Equippable = Weapon Item | Armor Item | Ring Item

weapons :: [Item]
weapons =
  [ Item "Dagger"     8  4 0
  , Item "Shortsword" 10 5 0
  , Item "Warhammer"  25 6 0
  , Item "Longsword"  40 7 0
  , Item "Greataxe"   74 8 0
  ]

armory :: [Item]
armory =
  [ Item "Leather"    13  0 1
  , Item "Chainmail"  31  0 2
  , Item "Splintmail" 53  0 3
  , Item "Bandedmail" 75  0 4
  , Item "Platemail"  102 0 5
  ]

rings :: [Item]
rings =
  [ Item "Damage +1"  25  1 0
  , Item "Damage +2"  50  2 0
  , Item "Damage +3"  100 3 0
  , Item "Armor +1"   20  0 1
  , Item "Armor +2"   40  0 2
  , Item "Armor +3"   80  0 3
  ]

allKits :: [[Item]]
allKits = do
  w <- weapons
  hasArmor <- [True, False]
  a' <- armory
  numRings <- [0,1,2]
  let a = [a' | hasArmor]
  case numRings of
    0 -> pure $ [w] <> a
    1 -> rings >>= \r -> pure $ [w,r] <> a
    2 -> do
          r1 <- rings
          r2 <- L.delete r1 rings
          pure $ [w,r1,r2] <> a
    _ -> error "impossible"

heroKits :: [(Natural, Entity)]
heroKits = foldr ((:) . equipHero) [(0, hero)] allKits

equipHero :: [Item] -> (Natural, Entity)
equipHero = foldr go (0, hero)
  where
    go :: Item -> (Natural, Entity) -> (Natural, Entity)
    go Item{..} (c, e) =
      ( c + itemCost
      , e { damage = e.damage + itemDamage
          , armor = e.armor + itemArmor
          }
      )

winningKits :: [(Natural, Entity)] -> [(Natural, Entity)]
winningKits = mapMaybe \(n, e) ->
  let (t1, t2) = winTurns (e, boss)
  in if t1 >= t2 then Just (n, e) else Nothing

losingKits :: [(Natural, Entity)] -> [(Natural, Entity)]
losingKits = mapMaybe \(n, e) ->
  let (t1, t2) = winTurns (e, boss)
  in if t1 < t2 then Just (n, e) else Nothing

