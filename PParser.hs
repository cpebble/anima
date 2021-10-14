{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PParser where

-- import Data.ByteString(ByteString) 
import Data.Aeson as JS

data Color = Red | Blue
  deriving (Show)

data Actor = Actor
  { color :: Color,
    _x :: Int,
    _y :: Int
  }
  deriving (Show)

data Tile = Goal Color | Block | Air
  deriving (Show)

type TileMap = [[Tile]]

data Puzzle = Puzzle
  { name :: String,
    width :: Int,
    height :: Int,
    tiles :: [[Tile]],
    actors :: [Actor],
    optimalMoves :: Int
  }
  deriving (Show)

charToTile :: Char -> Tile
charToTile '.' = Air
charToTile 'r' = Goal Red
charToTile 'b' = Goal Blue
charToTile ' ' = Block
charToTile c = error $ "Encountered unexpected block in tiles: " <> [c]
strToColor :: [Char] -> Color
strToColor "red" = Red
strToColor "blue" = Blue
strToColor _ = error "Unknown color"

instance FromJSON Actor where
    parseJSON = JS.withObject "Actor" $ \v -> do
        colorS <- v .: "color"
        colorS_ <- parseJSON colorS
        let color = strToColor colorS_
        x <- v .: "x"
        y <- v .: "y"
        return Actor {color = color, _x=x, _y=y}

--actors :: Value -> JS.Parser [Actor]



instance FromJSON Puzzle where
  parseJSON = JS.withObject "Puzzle" $ \v -> do
    name <- v .: "name"
    width <- v .: "width"
    height <- v .: "height"
    tileList <- v .: "tiles"
    tiles_ <- parseJSON tileList
    let tiles = map (map charToTile) tiles_
    actorObj <- v .: "actors"
    actors <- parseJSON actorObj
    optimalMoves <- v .: "optimalMoves"
    return Puzzle {name = name, width = width, height = height, tiles = tiles, actors = actors, optimalMoves = optimalMoves}

-- decodePuzzleString :: ByteString -> Puzzle
decodePuzzleString inp = case decode inp of
    Just a -> a
    Nothing -> error "Couldn't parse"

testActorString = "{ \"color\": \"red\", \"x\": 0, \"y\": 0 }"
testActor :: Maybe Actor
testActor = decode testActorString

testPuzzleString = "{ \"name\": \"Line Dance\", \"width\": 3, \"height\": 1, \"tiles\": [ \"..r\"], \"actors\": [ { \"color\": \"red\", \"x\": 0, \"y\": 0 } ], \"optimalMoves\": 2 }"
testPuzzle :: Maybe Puzzle
testPuzzle = decode testPuzzleString
