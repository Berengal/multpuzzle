{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Games.MultPuzzle
  -- | Two random numbers, one three digits and one two digits long, are multiplied
  --together using long multiplication. The digits are then replaced with letters
  --and the goal is to figure out which digit each letter corresponds to.
  --
  --The puzzle is solved by guessing (letter, digit) pairs until there are no
  --unknown digits left.

  --Unknown digits are digits that appear in the puzzle but
  --have not yet been correctly assigned to a letter. There could therefore be
  --digits that are neither known nor unknown (i.e. they don't appear in the
  --puzzle at all), but this difference should not be visible
  ( Puzzle
  , newPuzzle
  , newPuzzleIO
  , isSolved
  , cheatSolve
  , guess
  , isValidGuess
  , letterNeedsGuessing
  , digitNeedsGuessing
  , lettersInPuzzle
  , toPuzzleStrings
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
--import System.Random as R
import qualified System.Random.Shuffle as R (shuffleM)
import Control.Monad
import Data.Maybe
import Data.List
import qualified Control.Monad.Random.Lazy as R

-- | The valid digits are all digits from 0 to 9 inclusive
validDigits = Set.fromList "0123456789"
-- | The valid letters are the ten capital letters from A to J inclusive
validLetters = Set.fromList "ABCDEFGHIJ"

-- | Multiplication puzzle
--
-- 
data Puzzle = Puzzle { firstDigits :: String
                     -- ^ The three digits of the first number
                     , secondDigits :: String
                     -- ^ The two digits of the second number
                     , firstMult :: String
                     -- ^ Result of multiplying the first number with the last
                     -- digit of the second number
                     , secondMult :: String
                     -- ^ Result of multiplying the first number with the first
                     -- digit of the second number
                     , finalDigits :: String
                     -- ^ Result of the entire multiplication
                     , digitMap :: Map.Map Char Char
                     -- ^ Maps digits to the letters assigned to them
                     , letterMap :: Map.Map Char Char
                     -- ^ Maps letters to the digits they're assigned to
                     , knownDigits :: Set.Set Char
                     -- ^ Digits that have been correctly guessed
                     , unknownDigits :: Set.Set Char
                     -- ^ Digits that have not been correctly guessed but are
                     -- present in the puzzle
                     --
                     -- Note that knownDigits and unknownDigits are not
                     -- complimentary. Both sets only contain digits that
                     -- actually appear in the puzzle, so there could be valid
                     }
  deriving (Show)

-- | Specialization to IO of `newPuzzle`
newPuzzleIO :: IO Puzzle
newPuzzleIO = R.evalRandIO newPuzzle

-- | Creates a new random puzzle
newPuzzle :: (R.MonadRandom m) => m Puzzle
newPuzzle = do
  firstDigits <- show @Int <$> R.getRandomR (100,999)
  secondDigits <- show @Int <$> R.getRandomR (10,99)
  letters <- R.shuffleM (Set.toList validLetters)
  let [x,y] = secondDigits
      firstN = read firstDigits
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
      digitMap = Map.fromList (zip (Set.toList validDigits) letters)
      letterMap = Map.fromList . map (\(x,y)->(y,x)) . Map.assocs $ digitMap
  return Puzzle {..}

-- | A puzzle is solved when there are no unknown digits
isSolved :: Puzzle -> Bool
isSolved Puzzle{..} = Set.null unknownDigits

cheatSolve :: Puzzle -> Puzzle
cheatSolve Puzzle{..} = Puzzle { knownDigits = validDigits
                               , unknownDigits = Set.empty
                               , ..
                               }

-- | If the guess is correct, returns the updates puzzle where the digit is
-- known to have been correctly guessed. Returns Nothing if the guess is wrong
-- or invalid
guess :: Puzzle -> (Char, Char) -- ^ (letter, digit) guessing letter = digit
      -> Maybe Puzzle
guess puzzle (l, d) = do
  l' <- Map.lookup d (digitMap puzzle)
  guard (l == l')
  return puzzle{ knownDigits = Set.insert d (knownDigits puzzle)
               , unknownDigits = Set.delete d (unknownDigits puzzle)
               }

-- | A valid guess is a guess where the guessed letter appears in the puzzle and the
-- digit it's assigned to is unknown, and the guessed digit is a valid digit and
-- not known
--
-- Note that the digit doesn't have to actually appear in the puzzle, it only
-- has to be a valid digit
isValidGuess :: Puzzle -> (Char, Char) -- ^ (letter, digit) guessing letter = digit
             -> Bool
isValidGuess p (l, d) = letterNeedsGuessing p l && digitNeedsGuessing p d

-- | A digit needs guessing if it's a valid digit (element of `validDigits`) and
-- the digit is not already known
digitNeedsGuessing :: Puzzle -> Char -> Bool
digitNeedsGuessing p d = elem d validDigits && not (elem d (knownDigits p))

-- | A letter needs guessing if it's a valid letter (element of `validLetters`)
-- and the digit the letter is assigned to is not already known
letterNeedsGuessing :: Puzzle -> Char -> Bool
letterNeedsGuessing p l = isJust do
  guard $ Set.member l validLetters
  d <- Map.lookup l (letterMap p)
  guard $ elem d (unknownDigits p)

-- | The set of all letters that appear in the puzzle, regardless of the state
-- of their assigned digits
lettersInPuzzle :: Puzzle -> Set.Set Char
lettersInPuzzle Puzzle{..} = Set.map (fromJust . flip Map.lookup digitMap)
                             $ (Set.union knownDigits unknownDigits)

-- digitsInPuzzle Puzzle{..} = Set.union knownDigits unknownDigits

-- | The five numbers of the puzzle, with the unknown digits replaced by their
-- letter representatives
toPuzzleStrings :: Puzzle
  -> (String, String, String, String, String) -- ^ (first, second, first
                -- multiplication, second multiplication, final result)
toPuzzleStrings Puzzle{..} = (m 3 firstDigits
                             ,m 2 secondDigits
                             ,m 4 firstMult
                             ,m 4 secondMult
                             ,m 5 finalDigits
                             )
  where m padTo ds =
          let s = replicate (padTo - l) '0' ++ ds
              l = length ds
          in [Map.findWithDefault d d (Map.withoutKeys digitMap knownDigits)
             | d <- s]

-- TODO Remove
testPuzzle = do
  a <- R.evalRandIO newPuzzle
  let b = a{knownDigits = Set.fromList "5234"}
  return (a,b)
