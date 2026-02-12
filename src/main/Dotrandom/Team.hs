module Dotrandom.Team(
    emptyTeam, heroSet, preHeroSet
  , PreTeam(pos1M, pos2M, pos3M, pos4M, pos5M, PreTeam)
  , Team(pos1, pos2, pos3, pos4, pos5, Team)
  ) where

import Dotrandom.Hero(Hero)

import qualified Data.Set as Set


data PreTeam
  = PreTeam { pos1M :: Maybe Hero
            , pos2M :: Maybe Hero
            , pos3M :: Maybe Hero
            , pos4M :: Maybe Hero
            , pos5M :: Maybe Hero
            }

data Team
  = Team { pos1 :: Hero
         , pos2 :: Hero
         , pos3 :: Hero
         , pos4 :: Hero
         , pos5 :: Hero
         }

emptyTeam :: PreTeam
emptyTeam = PreTeam Nothing Nothing Nothing Nothing Nothing

heroSet :: Team -> Set Hero
heroSet (Team p1 p2 p3 p4 p5) = Set.fromList [p1, p2, p3, p4, p5]

preHeroSet :: PreTeam -> Set Hero
preHeroSet (PreTeam p1 p2 p3 p4 p5) = Set.fromList $ catMaybes [p1, p2, p3, p4, p5]
