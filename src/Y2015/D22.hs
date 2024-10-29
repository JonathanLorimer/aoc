module Y2015.D22 where

import Numeric.Natural
import Data.Text (Text)
import qualified Data.Text as T
import Utils ((-.-), NatMin (..))
import Data.Tree
import Control.Arrow ((&&&))
import Data.List qualified as L
import GHC.Natural (minusNatural)
import Control.Monad.State (State, MonadState (..), modify)
import Control.Monad (when)

-------------------------------------------------------------------------
-- Gameplay
-------------------------------------------------------------------------

data GameState =
  GameState
    { totalManaSpent :: Natural
    , bossState :: Entity
    , heroState :: Entity
    } deriving (Eq, Show)

initialGameState :: GameState
initialGameState =
  GameState 
    { totalManaSpent = 0
    , bossState = boss
    , heroState = hero
    }

initialGameStates :: [(GameState, Spell)]
initialGameStates = (initialGameState,) <$> allSpells 

gameForest :: GameMode -> State (Maybe NatMin) [Tree (GameState, Spell, GameResult)]
gameForest mode = flip unfoldForestM initialGameStates \(seedState, currentSpell) -> do
  s <- get
  let currentState = runTurn mode seedState currentSpell
      result = if maybe False (\n -> currentState.totalManaSpent > getNatural n) s
                then Suspended currentState
                else checkGameState mode currentState

  when (result == Win) $ 
    modify (mappend . Just . NatMin $ currentState.totalManaSpent)

  pure ((currentState, currentSpell, result)
       , case result of
            Win -> []
            Loss -> []
            PossibleMoves spells -> (currentState,) <$> spells
            Suspended _ -> []
       )

data GameResult = Loss | Win | PossibleMoves [Spell] | Suspended GameState
  deriving (Eq, Show)

data GameMode = EasyMode | HardMode
  deriving (Eq, Show)

checkGameState :: GameMode -> GameState -> GameResult
checkGameState mode gs
  | gs.heroState.mana < minManaCost = Loss
  | mode == EasyMode && gs.heroState.hitpoints == 0 = Loss
  | mode == HardMode && gs.heroState.hitpoints <= 1 = Loss
  | gs.bossState.hitpoints == 0 = Win
  | otherwise = PossibleMoves $ filter 
    (uncurry (&&) . (hasEnoughMana &&& isApplicable))
    allSpells
  where
    hasEnoughMana :: Spell -> Bool
    hasEnoughMana = (gs.heroState.mana >=) .  manaCost 

    isEnding :: Maybe Natural -> Bool
    isEnding Nothing = True
    isEnding (Just 1) = True
    isEnding _ = False

    isApplicable :: Spell -> Bool
    isApplicable = \case
      MagicMissile -> True
      Drain -> True
      Shield -> isEnding . fmap count $ L.find (\e -> e.name == "Shielded") gs.heroState.effects 
      Poison -> isEnding . fmap count $ L.find (\e -> e.name == "Poisoned") gs.bossState.effects
      Recharge -> isEnding . fmap count $ L.find (\e -> e.name == "Recharging") gs.heroState.effects 

runTurn :: GameMode -> GameState -> Spell -> GameState
runTurn mode prevState spell  =
      -- Apply effects before player's turn
  let heroBeforeHerosTurn = applyEffects (prevState.heroState {
        hitpoints = case mode of 
                      EasyMode -> prevState.heroState.hitpoints 
                      HardMode -> prevState.heroState.hitpoints -.- 1
      })
      bossBeforeHerosTurn = applyEffects prevState.bossState
      
      -- Apply spell for hero's turn
      ( heroAfterHerosTurn, bossAfterHerosTurn ) = case spell of 
            MagicMissile -> magicMissile heroBeforeHerosTurn bossBeforeHerosTurn
            Drain -> drain heroBeforeHerosTurn bossBeforeHerosTurn
            Shield -> (shield 0 heroBeforeHerosTurn, bossBeforeHerosTurn)
            Poison -> poison heroBeforeHerosTurn bossBeforeHerosTurn
            Recharge -> (recharge heroBeforeHerosTurn, bossBeforeHerosTurn)      

      -- Apply effects for boss's turn
      heroBeforeBossTurn = applyEffects heroAfterHerosTurn
      bossAfterBossTurn = applyEffects bossAfterHerosTurn

      -- Apply damage for hero's turn
      heroAfterBossTurn = if bossAfterBossTurn.hitpoints > 0 
        then heroBeforeBossTurn {
          hitpoints = heroBeforeBossTurn.hitpoints -.- 
            max 1 (bossAfterBossTurn.damage -.- heroBeforeBossTurn.armor)
        }
        else heroBeforeBossTurn
  in GameState 
      { totalManaSpent = prevState.totalManaSpent + manaCost spell
      , bossState = bossAfterBossTurn
      , heroState = heroAfterBossTurn
      }

applyEffects :: Entity -> Entity
applyEffects e = foldr apply (e { effects = [] }) e.effects 
  where
    apply :: Effect -> Entity -> Entity
    apply effect entity = 
      let newEntity = effect.runEffect  entity
      in case minusNaturalMaybe effect.count 1 of 
          Nothing -> effect.onEffectEnd newEntity
          Just 0 -> effect.onEffectEnd newEntity
          Just newEffectCount -> 
            newEntity 
              { effects = effect { count = newEffectCount } 
                        : newEntity.effects 
              } 

-------------------------------------------------------------------------
-- Entities
-------------------------------------------------------------------------

data Entity =
  Entity
    { hitpoints :: Natural
    , damage :: Natural
    , armor :: Natural
    , mana :: Natural
    , effects :: [Effect]
    } deriving (Eq, Show)

data Effect = 
  Effect 
    { name :: Text
    , runEffect :: Entity -> Entity
    , onEffectEnd :: Entity -> Entity
    , count :: Natural
    }

instance Eq Effect where
  e1 == e2 = e1.name == e2.name && e1.count == e2.count

instance Show Effect where
  show e = "Effect " <> T.unpack e.name <> " " <> show e.count

heroHealth :: Natural
heroHealth = 50

hero :: Entity
hero =   
  Entity
    { hitpoints = heroHealth
    , damage = 0
    , armor = 0
    , mana = 500
    , effects = []
    } 

boss :: Entity
boss = 
  Entity
    { hitpoints = 55
    , damage = 8
    , armor = 0
    , mana = 0
    , effects = []
    } 

-------------------------------------------------------------------------
-- Spells
-------------------------------------------------------------------------

data Spell = MagicMissile | Drain | Shield | Poison | Recharge
  deriving (Eq, Ord, Show, Enum)

allSpells :: [Spell]
allSpells = enumFrom (toEnum 0)

minManaCost :: Natural
minManaCost = getNatural . foldMap (NatMin . manaCost) $ allSpells

manaCost :: Spell -> Natural
manaCost = \case
  MagicMissile -> 53
  Drain -> 73
  Shield -> 113
  Poison -> 173
  Recharge -> 229

magicMissile :: Entity -> Entity -> (Entity, Entity)
magicMissile caster castee = 
  ( caster { mana = minusNatural caster.mana (manaCost MagicMissile) }
  , castee { hitpoints = castee.hitpoints -.- damage }
  )
  where
    damage :: Natural
    damage = 4

drain :: Entity -> Entity -> (Entity, Entity)
drain caster castee = 
  ( caster { 
      mana = minusNatural caster.mana (manaCost Drain), 
      hitpoints = caster.hitpoints + damage
    }
  , castee { hitpoints = castee.hitpoints -.- damage }
  )
  where
    damage :: Natural
    damage = 2

poison :: Entity -> Entity -> (Entity, Entity)
poison caster castee = 
  ( caster { mana = minusNatural caster.mana (manaCost Poison) }
  , castee { effects = poisonEffect : castee.effects}
  )
  where
    poisonDamage :: Natural
    poisonDamage = 3

    poisonEffect :: Effect
    poisonEffect = Effect
      { name = "Poisoned"
      , runEffect = \e -> e { hitpoints = e.hitpoints -.- poisonDamage }
      , onEffectEnd = id
      , count = 6
      }

shield :: Natural -> Entity -> Entity
shield defaultArmor caster = 
  caster { 
      mana = minusNatural caster.mana (manaCost Shield), 
      effects = shieldEffect : caster.effects
    }
  where
    shieldBonus :: Natural
    shieldBonus = 7

    shieldEffect :: Effect
    shieldEffect = Effect
      { name = "Shielded"
      , runEffect = \e -> e { armor = shieldBonus }
      , onEffectEnd = \e -> e { armor = defaultArmor }
      , count = 6
      }

recharge :: Entity -> Entity
recharge caster = 
  caster { 
      mana = minusNatural caster.mana (manaCost Recharge), 
      effects = rechargeEffect : caster.effects
    }
  where
    rechargeBonus :: Natural
    rechargeBonus = 101

    rechargeEffect :: Effect
    rechargeEffect = Effect
      { name = "Recharging"
      , runEffect = \e -> e { mana = e.mana + rechargeBonus }
      , onEffectEnd = id
      , count = 5
      }
