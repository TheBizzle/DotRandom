{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Dotrandom.Banlist(badHeroes, goodHeroes, tooBadHeroes, tooGoodHeroes, unevaluateds) where

import qualified Data.Set as Set

import Dotrandom.Hero(Hero(..))


-- Having one of these as an ally makes the game too easy
tooGoodHeroes :: Set Hero
tooGoodHeroes
  = Set.fromList $
      [ ArcWarden
      , DeathProphet
      , Kunkka
      , Razor
      , ShadowFiend
      , Sniper
      , Terrorblade
      , Zeus
      ]

-- The AI usually has significant impact on these heroes
goodHeroes :: Set Hero
goodHeroes
  = Set.fromList $
      [ Axe
      , Beastmaster
      , Brewmaster
      , Bristleback
      , Earthshaker
      , EarthSpirit
      , Jakiro
      , LegionCommander
      , Lich
      , Lina
      , Mars
      , Medusa
      , MonkeyKing
      , Necrophos
      , OgreMagi
      , Pudge
      , Pugna
      , SandKing
      , ShadowDemon
      , Silencer
      , Slardar
      , Tidehunter
      , Venomancer
      , Viper
      , VoidSpirit
      , Warlock
      , Weaver
      , Windranger
      , WinterWyvern
      ]

-- The AI really doesn't get these guys, but it at least has some impact
-- in the game
badHeroes :: Set Hero
badHeroes
  = Set.fromList $
      [ AncientApparition
      , AntiMage
      , Batrider
      , BountyHunter
      , Broodmother
      , ChaosKnight
      , DarkSeer
      , Dazzle
      , Disruptor
      , DragonKnight
      , Juggernaut
      , Kez
      , Lion
      , Luna
      , Meepo
      , Mirana
      , Muerta
      , NaturesProphet
      , Phoenix
      , PrimalBeast
      , Omniknight
      , Ringmaster
      , ShadowShaman
      , Snapfire
      , Tusk
      , Ursa -- Standard game: Feeds for 40-50 minutes, then has enough items that he can't help but start contributing.
      , WitchDoctor
      ]

-- The AI is bad at these heroes and doesn't understand how to play their
-- core concept
tooBadHeroes :: Set Hero
tooBadHeroes
  = Set.fromList $
      [ Abaddon
      , Clinkz
      , Clockwerk
      , CrystalMaiden
      , DarkWillow
      , Enchantress
      , Enigma
      , FacelessVoid
      , Grimstroke
      , Gyrocopter
      , ElderTitan
      , Io
      , KeeperOfTheLight
      , Lifestealer
      , LoneDruid
      , Marci
      , Morphling
      , NyxAssassin
      , Oracle
      , Pangolier
      , PhantomAssassin
      , Riki
      , Rubick
      , Slark
      , Spectre -- Doesn't seem like it even knows how to press the buttons on the reworked Spectre
      , SpiritBreaker
      , Sven
      , TemplarAssassin
      , TrollWarlord
      , Underlord
      , VengefulSpirit
      , Visage
      ]

historicBads :: Set Hero
historicBads
  = Set.fromList $
      [ Doom
      , Hoodwink
      , Invoker
      , Magnus
      , NagaSiren
      , PhantomLancer
      , Puck
      ]

unevaluateds :: Set Hero
unevaluateds
  = Set.fromList $
      [ Alchemist
      , Bane
      , Bloodseeker
      , Magnus
      , NightStalker
      , PhantomLancer
      , Puck
      , SkywrathMage
      , TreantProtector
      , Undying
      , WraithKing

      , CentaurWarrunner
      , Chen
      , Dawnbreaker
      , Doom
      , Drow
      , EmberSpirit
      , Hoodwink
      , Huskar
      , Invoker
      , Largo
      , Leshrac
      , Lycan
      , NagaSiren
      , OutworldDestroyer
      , QueenOfPain
      , StormSpirit
      , Techies
      , Timbersaw
      , Tinker
      , Tiny
      ]
