module Bindings.Workspaces where

import XMonad
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Loggers
import Data.Maybe

workspace :: [(String, X())]
workspace = -- grouped workspaces
            [ ("M-<Up>", (switchToGroup "next"))
            , ("M-<Down>", (switchToGroup "prev"))
            , ("M-<Left>", (switchInGroup "next"))
            , ("M-<Right>", (switchInGroup "prev"))

            -- normal workspaces
            , ("M-S-<Left>", prevWS)
            , ("M-S-<Right>", nextWS)
            , ("M-C-<Left>", shiftToPrev >> prevWS)
            , ("M-C-<Right>", shiftToNext >> nextWS)
            , ("M-<Tab>", toggleWS)
            ]


switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . W.greedyView


wsBy :: Int -> X (WorkspaceId)
wsBy = findWorkspace getSortByIndex Next anyWS


nbGroups :: Int
nbGroups = 3


-- create groups within which you can shift
switchInGroup direction = do
  x <- logCurrent
  let y = read $ fromJust x -- read converts string -> int

  if direction == "next"
    then do
      if y `mod` nbGroups /= 1 then prevWS else switchWorkspace $ nbGroups - 1
   
  else if direction == "prev"
    then do
      if y `mod` nbGroups /= 0 then nextWS else switchWorkspace $ 1 - nbGroups

  else pure ()


switchToGroup direction = do
  if direction == "next" then switchWorkspace $ nbGroups
  else if direction == "prev" then switchWorkspace $ -nbGroups
  else pure ()


-- shift window to workspace and follow it
shift'n'view i = W.greedyView i . W.shift i
