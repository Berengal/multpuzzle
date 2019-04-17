{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Games.MultPuzzle as P
import Brick
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.String
import Lens.Micro.Platform
import Graphics.Vty.Attributes
import Graphics.Vty

import Control.Monad.Random.Lazy

type GameEvent = ()
type GameName = ()


data GameState = GameState { _puzzle :: P.Puzzle
                           , _guesses :: Int
                           , _message :: String
                           , _chosenLetter :: Maybe Char
                           }

makeLenses ''GameState


ui :: Widget ()
ui = withBorderStyle unicode $
     borderWithLabel (str "Hello") $
     (center (str "Left") <+> vBorder <+> center (str "Right"))
     
main :: IO _
main = do
  p <- P.newPuzzleIO
  defaultMain gameApp (initialGameState p)

initialGameState puzzle = GameState puzzle 0 "Hello" Nothing

gameApp :: App GameState GameEvent GameName
gameApp = App {..}
  where appChooseCursor = neverShowCursor
        appDraw = return . appView
        appHandleEvent = handleEvent
        appStartEvent = return
        appAttrMap = const $ attrMap defAttr textAttributes

textAttributes = [(attrName "text" <> attrName "good", fgColor green)
                 ,(attrName "text" <> attrName "bad" , fgColor red)
                 ,(attrName "text" <> attrName "meh" , fgColor yellow)
                 ]
  where fgColor c = Attr KeepCurrent (SetTo c) KeepCurrent KeepCurrent
        
handleEvent s (MouseDown _ _ _ _) = continue s
handleEvent s (MouseUp _ _ _) = continue s
handleEvent s (AppEvent _) = error "TODO Handle custom events when they're added" -- TODO
handleEvent s (VtyEvent (EvKey k _modifiers)) =
  let p = s^.puzzle
      cl = s^.chosenLetter
      hasLetter = isJust cl
  in case k of
    KChar 'q' -> halt s
    KChar c
      | not hasLetter && P.letterNeedsGuessing p (toUpper c)
        -> continue (s & chosenLetter .~ Just c
                      & message .~ "")
      | not hasLetter && not (P.letterNeedsGuessing p (toUpper c))
        -> continue (s & message.~ "Not an unknown letter: " ++ (show c))
      | hasLetter && not (P.digitNeedsGuessing p c)
        -> continue (s & chosenLetter .~ Nothing
                      & message .~ "Not an unknown digit: " ++ (show c))
      | hasLetter && P.digitNeedsGuessing p c
        -> case P.guess p (toUpper (fromJust cl), c) of
          Nothing -> continue (s & guesses +~ 1
                                & message .~ "You guessed wrong: " ++ show (fromJust cl, c)
                                & chosenLetter .~ Nothing)
          Just p' -> continue (s & puzzle .~ p'
                               & message .~ "You guessed correct!"
                               & chosenLetter .~ Nothing)
      | otherwise -> continue s
handleEvent s (VtyEvent _) = continue s


appView app = setAvailableSize (30, 30)
              $ hCenter (hLimit 15 (puzzleView (app ^. puzzle)))
              <=> hCenter (renderPrompt (app ^.chosenLetter))
              <=> hCenter (str (app ^.message))
  where renderPrompt Nothing = str " "
        renderPrompt (Just c) = str (toUpper c: " = ")

puzzleView :: P.Puzzle -> Widget n
puzzleView puzzle = border . vBox $ puzzleStrings
  where f = padLeft Max . hBox . map greenDigits . concatMap (\c -> [' ', c])
        puzzleStrings = let (a, b, c, d, e) = P.toPuzzleStrings puzzle
                        in [ f a, f (b)
                           , numSep
                           , f c, f (d ++ " ")
                           , numSep
                           , f e]
        numSep = withBorderStyle (borderStyleFromChar '-') hBorder
        greenDigits c | elem c ['0'..'9'] =
                          withAttr (attrName "text" <> attrName "good") (str (c:[]))
                      | otherwise = str (c:[])

        

guess :: GameState -> (Char, Char) -> Maybe GameState
guess state g = puzzle (flip P.guess g) state
