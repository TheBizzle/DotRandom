module Main(main) where

import Data.Maybe(fromJust)
import Data.Text(breakOn, splitOn)

import System.Random(randomRIO)

import Dotrandom.Hero(Hero)
import Dotrandom.HeroLists(badHeroes, goodHeroes, tooBadHeroes, tooGoodHeroes, unevaluateds)
import Dotrandom.InternalName(toInternalName)
import Dotrandom.Positions(Position(Pos1, Pos2, Pos3, Pos4, Pos5), positions)
import Dotrandom.Team(emptyTeam, heroSet, preHeroSet, PreTeam(pos1M, pos2M, pos3M, pos4M, pos5M, PreTeam), Team(Team))

import qualified Data.List    as List
import qualified Data.Map     as Map
import qualified Data.Set     as Set
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO


main :: IO ()
main =
  do
    TIO.putStrLn "Enter your reserved heroes (2: ShadowFiend, 5: Hoodwink)"
    input    <- TIO.getLine
    let team  = parseReserves input

    let allHeroes      = [minBound..maxBound] :: [Hero]
    let fullPool       = Map.fromList $ map (, error "No weight given") allHeroes
    let reservedHeroes = preHeroSet team
    let weightedPool   = weightPool fullPool [ (     badHeroes, 25)
                                             , (    goodHeroes, 10)
                                             , (reservedHeroes,  0)
                                             , (  tooBadHeroes,  0)
                                             , ( tooGoodHeroes,  0)
                                             , (  unevaluateds, 80)
                                             ]

    mainLoop weightedPool team

mainLoop :: Map Hero Word -> PreTeam -> IO ()
mainLoop weightedPool team =
  do
    tryDrafting weightedPool team
    TIO.putStrLn "\nType 'r' if you want to re-roll the drafts"
    input <- TIO.getLine
    if (Text.toLower $ Text.strip input) == "r" then
      mainLoop weightedPool team
    else
      return ()

tryDrafting :: Map Hero Word -> PreTeam -> IO ()
tryDrafting weightedPool team =
  do
    let direPool    = weightedPool
    direTeam       <- draft direPool emptyTeam
    let direHeroes  = heroSet direTeam

    let finalPool = weightPool weightedPool [ (    badHeroes, 15)
                                            , (   goodHeroes, 20)
                                            , ( tooBadHeroes,  0)
                                            , (tooGoodHeroes, 20)
                                            , (   direHeroes,  0)
                                            , ( unevaluateds, 80)
                                            ]

    radiantTeam <- draft finalPool team

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

draft :: Map Hero Word -> PreTeam -> IO Team
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
        let notInPool  = Set.difference (Map.keysSet currPool) posHeroes
        let finalPool  = weightPool currPool [(notInPool, 0)]
        choice        <- randomOneOf finalPool
        let newTeam    = updatePos pteam pos choice
        let newPool    = Map.insert choice 0 currPool
        return (newTeam, newPool)

isFilledIn :: Position -> PreTeam -> Bool
isFilledIn Pos1 (PreTeam p _ _ _ _) = isJust p
isFilledIn Pos2 (PreTeam _ p _ _ _) = isJust p
isFilledIn Pos3 (PreTeam _ _ p _ _) = isJust p
isFilledIn Pos4 (PreTeam _ _ _ p _) = isJust p
isFilledIn Pos5 (PreTeam _ _ _ _ p) = isJust p

weightPool :: Map Hero Word -> [(Set Hero, Word)] -> Map Hero Word
weightPool = foldr $ \(set, weight) pool ->
  Set.foldr (\hero p -> Map.insert hero weight p) pool set

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

randomOneOf :: Map a Word -> IO a
randomOneOf m =
  do
    let weightedPairs  = Map.toList m
    let total          = sum $ map snd weightedPairs
    let slottedPairs   = List.scanl1 (\(_, acc) (x, w) -> (x, acc + w)) weightedPairs
    drawnNum          <- randomRIO (0, total)
    case find (\(_, c) -> c >= drawnNum) slottedPairs of
      Just (x, _) -> return x
      Nothing     -> error "Can't get random elem from map of empty weights"
