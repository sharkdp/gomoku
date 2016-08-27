module Gomoku where

import Prelude

import Data.GameTree (class Node, Score(..))
import Data.List (List, (:), (..), null, filter)
import Data.Map (Map, lookup, insert, toList)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.Pair (Pair, (~))
import Data.Tuple (Tuple(..), snd)
import Data.Traversable (find)
import Data.Foldable (fold)

-- Markers

data Marker = X | O

derive instance eqMarker ∷ Eq Marker

instance showMarker ∷ Show Marker where
  show X = "X"
  show O = "O"

next ∷ Marker → Marker
next X = O
next O = X

-- Coordinates

type Coord = Pair Int

-- Board

data Board = Board
  { size     ∷ Int
  , map      ∷ Map Coord Marker
  , markersX ∷ List Coord
  , markersO ∷ List Coord
  }

size ∷ Board → Int
size (Board b) = b.size

instance showBoard ∷ Show Board where
  show board = fold do
    r ← 0 .. (size board - 1)
    let line = fold do
          c ← 0 .. (size board - 1)
          pure $
            case markerAt board (r ~ c) of
              Just m → show m
              Nothing → "."
    pure $ line <> "\n"

markers ∷ Board → Marker → List Coord
markers (Board b) X = b.markersX
markers (Board b) O = b.markersO

addMarker ∷ Board → Coord → Marker → Board
addMarker (Board b) coord marker = Board $
  b { map = insert coord marker b.map
    , markersO = if marker == X then coord : b.markersO else b.markersO
    , markersX = if marker == X then coord : b.markersX else b.markersX
    }

markerAt ∷ Board → Coord → Maybe Marker
markerAt (Board b) c = lookup c b.map

emptyBoard ∷ Int → Board
emptyBoard s =
  Board { size: s, map: mempty, markersX: mempty, markersO: mempty }

occupiedCoords ∷ Board → List Coord
occupiedCoords (Board b) = b.markersO <> b.markersX

allCoords ∷ Board → List Coord
allCoords board = do
  r ← 0 .. (size board - 1)
  c ← 0 .. (size board - 1)
  pure (r ~ c)

freeCoords ∷ Board → List Coord
freeCoords board = filter (isFree board) (allCoords board)

isOccupied ∷ Board → Coord → Boolean
isOccupied b c = isJust (markerAt b c)

isFree ∷ Board → Coord → Boolean
isFree = not <<< isOccupied

hasRun ∷ Board → Maybe Marker
hasRun (Board b) = snd <$> find runFrom (toList b.map)
  where
    runFrom (Tuple coord marker) = true -- TODO

-- Game state

data GameState = GameState
  { board ∷ Board
  , player ∷ Marker
  }

instance showGameState ∷ Show GameState where
  show (GameState s) = "Next player: " <> show s.player <> "\n" <> show s.board

initialState ∷ Int → GameState
initialState s = GameState
  { board: emptyBoard s
  , player: X
  }

instance nodeBoard ∷ Node GameState where
  isTerminal (GameState s) = null (freeCoords s.board)

  score (GameState s) =
    case (hasRun s.board) of
      Just player → if player == s.player then Win else Lose
      Nothing → heuristic s.board s.player
    where
      heuristic board marker = Score 0.0

  children (GameState s) = toGameState <$> freeCoords s.board
    where toGameState c =
            GameState { board: addMarker s.board c s.player
                      , player: next s.player }
