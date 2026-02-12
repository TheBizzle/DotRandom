module Main(main) where

import Data.List((!!))
import Data.Maybe(fromJust)
import Data.Text(breakOn, splitOn)

import System.Random(randomRIO)

import Dotrandom.Banlist(bothBans, direBans, radiantBans)
import Dotrandom.Hero(Hero)
import Dotrandom.InternalName(toInternalName)
import Dotrandom.Positions(Position(Pos1, Pos2, Pos3, Pos4, Pos5), positions)
import Dotrandom.Team(emptyTeam, heroSet, preHeroSet, PreTeam(pos1M, pos2M, pos3M, pos4M, pos5M, PreTeam), Team(Team))

import qualified Data.List    as List
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO


main :: IO ()
main =
  do
    TIO.putStrLn "Enter your reserved heroes (2: ShadowFiend, 5: Hoodwink)"
    input    <- TIO.getLine
    let team  = parseReserves input

    let allHeroes      = Set.fromList ([minBound..maxBound] :: [Hero])
    let reservedHeroes = preHeroSet team
    let cpuPool        = Set.difference allHeroes reservedHeroes
    let bothPool       = Set.difference cpuPool   bothBans

    mainLoop bothPool team

mainLoop :: Set Hero -> PreTeam -> IO ()
mainLoop bothPool team =
  do
    tryDrafting bothPool team
    TIO.putStrLn "\nType 'r' if you want to re-roll the drafts"
    input <- TIO.getLine
    if (Text.toLower $ Text.strip input) == "r" then
      mainLoop bothPool team
    else
      return ()

tryDrafting :: Set Hero -> PreTeam -> IO ()
tryDrafting bothPool team =
  do
    let direPool    = Set.difference bothPool direBans
    direTeam       <- draft direPool emptyTeam
    let direHeroes  = heroSet direTeam

    let radiantPool  = Set.difference bothPool    radiantBans
    let finalPool    = Set.difference radiantPool direHeroes
    radiantTeam     <- draft finalPool team

    outputTeams radiantTeam direTeam

parseReserves :: Text -> PreTeam
parseReserves input = team
  where
    reservations = splitOn "," input
    textPairs    = map (breakOn ":") reservations
    team         = foldr fillIn emptyTeam textPairs

    t = Text.strip

    fillIn :: (Text, Text) -> PreTeam -> PreTeam
    fillIn (n, h) acc = updatePos acc (readPos $ t n) (read (asString $ t $ Text.tail $ t h) :: Hero)

    readPos "1" = Pos1
    readPos "2" = Pos2
    readPos "3" = Pos3
    readPos "4" = Pos4
    readPos "5" = Pos5
    readPos   x = error $ "Invalid position value: " <> x

updatePos :: PreTeam -> Position -> Hero -> PreTeam
updatePos team Pos1 h = team { pos1M = Just h }
updatePos team Pos2 h = team { pos2M = Just h }
updatePos team Pos3 h = team { pos3M = Just h }
updatePos team Pos4 h = team { pos4M = Just h }
updatePos team Pos5 h = team { pos5M = Just h }

draft :: Set Hero -> PreTeam -> IO Team
draft pool preteam =
  do
    let posPools  = map toPosPair ([minBound..maxBound] :: [Position])
    (t, _)       <- foldlM draftIfOpen (preteam, pool) posPools
    return $ Team (fromJust t.pos1M) (fromJust t.pos2M) (fromJust t.pos3M) (fromJust t.pos4M) (fromJust t.pos5M)
  where
    toPosPair p@Pos1 = case positions of (p1,  _,  _,  _,  _) -> (p, p1)
    toPosPair p@Pos2 = case positions of ( _, p2,  _,  _,  _) -> (p, p2)
    toPosPair p@Pos3 = case positions of ( _,  _, p3,  _,  _) -> (p, p3)
    toPosPair p@Pos4 = case positions of ( _,  _,  _, p4,  _) -> (p, p4)
    toPosPair p@Pos5 = case positions of ( _,  _,  _,  _, p5) -> (p, p5)

    draftIfOpen (pteam, currPool) (pos, posHeroes) =
      if pos `isFilledIn` pteam then
        return (pteam, currPool)
      else do
        let finalPool  = Set.intersection currPool posHeroes
        choice        <- randomOneOf finalPool
        let newTeam    = updatePos pteam pos choice
        let newPool    = Set.delete choice currPool
        return (newTeam, newPool)

isFilledIn :: Position -> PreTeam -> Bool
isFilledIn Pos1 (PreTeam p _ _ _ _) = isJust p
isFilledIn Pos2 (PreTeam _ p _ _ _) = isJust p
isFilledIn Pos3 (PreTeam _ _ p _ _) = isJust p
isFilledIn Pos4 (PreTeam _ _ _ p _) = isJust p
isFilledIn Pos5 (PreTeam _ _ _ _ p) = isJust p

outputTeams :: Team -> Team -> IO ()
outputTeams radiant dire =
  do
    TIO.putStrLn ""
    outputTeam radiant
    TIO.putStrLn "====="
    outputTeam dire

outputTeam :: Team -> IO ()
outputTeam (Team p1 p2 p3 p4 p5) =
  [p1, p2, p3, p4, p5] |>
    map toInternalName &>
    map (\name -> "  '" <> name <> "',") &>
    flip forM_ TIO.putStrLn

randomOneOf :: Set a -> IO a
randomOneOf s
  | Set.null s = error "Can't get random elem from set of nothing"
  | otherwise  =
    do
      let items       = Set.toList s
      let upperBound  = (List.length items) - 1
      i              <- randomRIO (0, upperBound)
      return $ items !! i
