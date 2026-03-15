{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Dotrandom.HeroLists(badHeroes, goodHeroes, mediocreHeroes, tooBadHeroes, tooGoodHeroes, unevaluateds) where

import qualified Data.Set as Set

import Dotrandom.Hero(Hero(..))


-- Having one of these as an ally makes the game too easy
tooGoodHeroes :: Set Hero
tooGoodHeroes
  = Set.fromList $
      [ ArcWarden
      , DeathProphet
      , Kunkka
      , Lich
      , OutworldDestroyer
      , Razor
      , ShadowFiend
      , SkywrathMage
      , Sniper
      , Terrorblade
      , Tiny
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
      , CentaurWarrunner
      , Earthshaker
      , EarthSpirit
      , Huskar
      , Jakiro
      , LegionCommander
      , Lina
      , Lycan
      , NagaSiren
      , Mars
      , Medusa
      , MonkeyKing
      , Necrophos
      , OgreMagi
      , Pudge
      , Pugna
      , QueenOfPain
      , ShadowDemon
      , Slardar
      , Tidehunter
      , Timbersaw
      , Venomancer
      , VoidSpirit
      , Warlock
      , Windranger
      , WinterWyvern
      ]

-- They're bad and clueless.  But they have some redeeming moments.
mediocreHeroes :: Set Hero
mediocreHeroes
  = Set.fromList $
      [ Bane
      , BountyHunter
      , Bloodseeker
      , Dawnbreaker
      , EmberSpirit
      , Invoker
      , Kez
      , Leshrac
      , Lion
      , Magnus
      , Muerta
      , Omniknight
      , PhantomLancer
      , Ringmaster
      , SandKing
      , Silencer
      , Snapfire
      , StormSpirit
      , Viper
      , Weaver
      ]

-- The AI really doesn't get these guys, but it at least has some impact
-- in the game
badHeroes :: Set Hero
badHeroes
  = Set.fromList $
      [ AncientApparition
      , AntiMage
      , Batrider
      , Broodmother
      , ChaosKnight
      , DarkSeer
      , Dazzle
      , Disruptor
      , Doom
      , DragonKnight
      , DrowRanger
      , Hoodwink
      , Juggernaut
      , Luna
      , Meepo
      , Mirana
      , NaturesProphet
      , NightStalker
      , Phoenix
      , PrimalBeast -- Mediocre in a good game.  In a bad game, sits in base the whole game, randomly gyrating in the direction of the fountain
      , ShadowShaman
      , Techies
      , Tinker
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
      , Alchemist -- Finishes his Radiance at 33 minutes; finishes the match with barely more net worth (and less hero damage done) than the pos 5
      , Chen
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
      , Puck
      , Riki
      , Rubick
      , Slark
      , Spectre -- Doesn't seem like it even knows how to press the buttons on the reworked Spectre
      , SpiritBreaker
      , Sven
      , TemplarAssassin
      , TreantProtector
      , TrollWarlord
      , Underlord
      , Undying
      , VengefulSpirit
      , Visage
      , WraithKing
      ]

unevaluateds :: Set Hero
unevaluateds
  = Set.fromList $
      [ Largo
      ]
