{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Games.MultPuzzle
  ( Puzzle (..)
  , newPuzzle
  , isSolved
  , guess
  , toPuzzleStrings
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
--import System.Random as R
import qualified System.Random.Shuffle as R (shuffleM)
import qualified Control.Monad.Random.Lazy as R

validDigits = "0123456789"
validLetters = "ABCDEFGHIJ"

data Puzzle = Puzzle { firstDigits :: String
                     , secondDigits :: String
                     , firstMult :: String
                     , secondMult :: String
                     , finalDigits :: String
                     , digitMap :: Map.Map Char Char
                     , knownDigits :: Set.Set Char
                     , unknownDigits :: Set.Set Char
                     , wrongGuesses :: Int
                     }
  deriving (Show)

newPuzzle :: (R.MonadRandom m, R.MonadInterleave m) => m Puzzle
newPuzzle = do
  firstDigits <- take 3 <$> R.interleave (R.getRandomRs ('0', '9'))
  secondDigits <- take 2 <$> R.interleave (R.getRandomRs ('0', '9'))
  letters <- R.shuffleM validLetters
  let [x,y] = secondDigits
  
  let firstN = read firstDigits
      firstMult = show $ firstN * read [y]
      secondMult = show $ firstN * read [x]
      finalDigits = show (firstN * read secondDigits)
      knownDigits = Set.empty
      unknownDigits = Set.fromList $ concat [ firstDigits
                                            , secondDigits
                                            , firstMult
                                            , secondMult
                                            , finalDigits
                                            ]
      digitMap = Map.fromList (zip validDigits letters)
      wrongGuesses = 0
  return Puzzle {..}

isSolved :: Puzzle -> Bool
isSolved Puzzle{..} = Set.null unknownDigits

guess :: Puzzle -> (Char, Char) -> (Puzzle, Bool)
guess puzzle (l, d) = case Map.lookup d (digitMap puzzle) of
                        Nothing -> (puzzle, False)
                        Just l' | l == l' ->
                                    (puzzle{ knownDigits = Set.insert d (knownDigits puzzle)
                                           , unknownDigits = Set.delete d (unknownDigits puzzle)
                                           }
                                    , True)
                                | otherwise -> (puzzle{ wrongGuesses = wrongGuesses puzzle + 1}
                                               , False)

toPuzzleStrings :: Puzzle -> (String, String, String, String, String)
toPuzzleStrings Puzzle{..} = (m firstDigits
                             ,m secondDigits
                             ,m firstMult
                             ,m secondMult
                             ,m finalDigits
                             )
  where m = map \d -> Map.findWithDefault d d (Map.withoutKeys digitMap knownDigits)

testPuzzle = do
  a <- R.evalRandIO newPuzzle
  let b = a{knownDigits = Set.fromList "5234"}
  return (a,b)
