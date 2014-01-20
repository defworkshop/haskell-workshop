module Main where

import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.BitSet.Dynamic as BS

data Entry = NN | I1 | I2 | I3 | I4 | I5 | I6 | I7 | I8 | I9 deriving (Eq,Ord,Enum)

instance Show Entry where
  show NN = " "
  show I1 = "1"
  show I2 = "2"
  show I3 = "3"
  show I4 = "4"
  show I5 = "5"
  show I6 = "6"
  show I7 = "7"
  show I8 = "8"
  show I9 = "9"

type Game = V.Vector (V.Vector Entry)

showGame :: Game -> String
showGame = concat . interl rowSep . V.toList . V.map showRow
  where
    nl    = "\n"
    sep   = "\n---------------------\n"
    colSep = [" ", " ", " | ", " ", " ", " | ", " ", " "]
    rowSep = [nl, nl, sep, nl, nl, sep, nl, nl]
    showRow r = concat . interl colSep . V.toList $ V.map show r
    interl l r = concat . L.transpose $ [r,l]

game :: Game
game = V.fromList [ V.fromList [I5, I3, NN, NN, I7, NN, NN, NN, NN]
                  , V.fromList [I6, NN, NN, I1, I9, I5, NN, NN, NN]
                  , V.fromList [NN, I9, I8, NN, NN, NN, NN, I6, NN]
                  , V.fromList [I8, NN, NN, NN, I6, NN, NN, NN, I3]
                  , V.fromList [I4, NN, NN, I8, NN, I3, NN, NN, I1]
                  , V.fromList [I7, NN, NN, NN, I2, NN, NN, NN, I6]
                  , V.fromList [NN, I6, NN, NN, NN, NN, NN, I2, I8]
                  , V.fromList [NN, NN, NN, I4, I1, I9, NN, NN, I5]
                  , V.fromList [NN, NN, NN, NN, I8, NN, NN, I7, I9]]

get :: Game -> Int -> Int -> Entry
get g r c = g V.! r V.! c

set :: Game -> Int -> Int -> Entry -> Game
set g r c e = g V.// [(r, g V.! r V.// [(c,e)])]

noRepeats :: V.Vector Entry -> Bool
noRepeats = loop BS.empty
  where
    loop _  v | V.null v       = True
    loop bs v | V.head v == NN = loop bs (V.tail v)
    loop bs v                  = not (BS.member (V.head v) bs)
                                 && loop (BS.insert (V.head v) bs) (V.tail v)

column :: Game -> Int -> V.Vector Entry
column g c = V.generate 9 (\r -> g V.! r V.! c)

row :: Game -> Int -> V.Vector Entry
row g r = g V.! r

square :: Game -> Int -> Int -> V.Vector Entry
square g r c = V.slice col 3 (g V.! row) V.++ V.slice col 3 (g V.! (row+1)) V.++ V.slice col 3 (g V.! (row+2))
  where
    row = r*3
    col = c*3

gameValid :: Game -> Bool
gameValid g = L.foldl' (\acc v -> acc && noRepeats v) True (columns ++ rows ++ squares)
  where
    columns = [column g c | c <- [0..8]]
    rows    = [row g r | r <- [0..8]]
    squares = [square g r c | r <- [0..2], c <- [0..2]]

emptySquares :: Game -> [(Int,Int)]
emptySquares g = [(toRow sq roff, toCol sq coff) |
                  sq <- [(sr,sc) | sr <- [0..2], sc <- [0..2]],
                  roff <- [0..2],
                  coff <- [0..2],
                  get g (toRow sq roff) (toCol sq coff) == NN]
  where
    toRow sq r = fst sq * 3 + r
    toCol sq c = snd sq * 3 + c

solve :: Game -> [Game]
solve g = if null esquares then [g] else concatMap solve tries
  where
    esquares = (emptySquares g)
    tries = filter gameValid [set g r c e | e <- [I1 .. I9], (r,c) <- take 1 esquares]

main :: IO ()
main = putStrLn . L.intercalate "\n\n" . map showGame $ solve game
