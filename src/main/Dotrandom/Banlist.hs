{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Dotrandom.Banlist(radiantBans, direBans, bothBans) where

import qualified Data.Set as Set

import Dotrandom.Hero(Hero(..))


-- Having one of these as an ally makes the game too easy
radiantBans :: Set Hero
radiantBans
  = Set.fromList $
      [ ArcWarden
      , DragonKnight
      , ShadowFiend
      , Sniper
      , Zeus
      ]

direBans :: Set Hero
direBans
  = Set.fromList $
      [
      ]

-- The AI is bad at these heroes and doesn't understand how to play their core concept
bothBans :: Set Hero
bothBans
  = Set.fromList $
      [ AntiMage
      , Batrider
      , Broodmother
      , Clinkz
      , Clockwerk
      , Dazzle
      , Disruptor
      , Doom
      , Earthshaker
      , Enigma
      , FacelessVoid
      , NaturesProphet
      , Grimstroke
      , Hoodwink
      , Invoker
      , Jakiro
      , Juggernaut
      , KeeperOfTheLight
      , Kez
      , LegionCommander
      , Lifestealer
      , Lina
      , Lion
      , Magnus
      , Mirana
      , MonkeyKing
      , Muerta
      , NagaSiren
      , NyxAssassin
      , Oracle
      , PhantomAssassin
      , PhantomLancer
      , Puck
      , Rubick
      , Slark
      , Spectre
      , SpiritBreaker
      , Sven
      , Terrorblade
      , TrollWarlord
      , WinterWyvern
      ]
