module Bindings.Workspaces where

import XMonad
import XMonad.Actions.SwapWorkspaces (swapTo)
import XMonad.Actions.CycleWS (shiftToPrev, shiftToNext, prevWS, nextWS, toggleWS, findWorkspace, anyWS, Direction1D(Next, Prev))
import qualified XMonad.StackSet as W
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Loggers
import Data.Maybe (fromJust)

workspace :: [(String, X())]
workspace = -- grouped workspaces
            [ ("M-<Up>", (switchToGroup "next"))
            , ("M-<Down>", (switchToGroup "prev"))
            , ("M-C-<Up>", (switchToGroupIncludingScratch "next"))
            , ("M-C-<Down>", (switchToGroupIncludingScratch "prev"))

            , ("M-<Left>", (switchInGroup "next"))
            , ("M-<Right>", (switchInGroup "prev"))

            -- watch -n 1 xdotool key "Super_L+z" # refresh SINE
            , ("M-z", (switchInGroup "prev") >> (switchInGroup "next"))

            , ("M-u", (switchToGroup "next"))
            , ("M-n", (switchInGroup "next"))
            , ("M-i", (switchInGroup "prev"))
            , ("M-,", (switchToGroup "prev"))
            , ("M-C-n", shiftToPrev >> prevWS)
            , ("M-C-i", shiftToNext >> nextWS)

            -- normal workspaces
            , ("M-S-<Left>", prevWS)
            , ("M-S-<Right>", nextWS)
            , ("M-C-<Left>", shiftToPrev >> prevWS)
            , ("M-C-<Right>", shiftToNext >> nextWS)
            , ("M-r", shiftToPrev >> prevWS)
            , ("M-t", shiftToNext >> nextWS)
            , ("M-<Tab>", toggleWS)

            -- swap workspaces
            , ("M-M1-<Right>", swapTo Next)
            , ("M-M1-<Left>", swapTo Prev)

            -- move all windows to workspace 1
            -- , ("M-C-o", gets windowset >>= mapM_ (windows . W.shiftWin "1") . W.allWindows)

            -- doShiftX ws = ask >>= \w -> liftX $ Endo . (`W.shiftWin` w) <$> ws
            ]


switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . W.greedyView


wsBy :: Int -> X (WorkspaceId)
wsBy = findWorkspace getSortByIndex Next anyWS


groupSize :: Int
groupSize = 3

-- create groups within which you can shift
switchInGroup direction = do
  x <- logCurrent -- current workspace
  let y = read $ fromJust x -- read converts string -> int

  if direction == "next"
    then do
      if y `mod` groupSize /= 1 then prevWS else switchWorkspace $ groupSize - 1

  else if direction == "prev"
    then do
      if y `mod` groupSize /= 0 then nextWS else switchWorkspace $ 1 - groupSize

  else pure ()


nbGroups :: Int
nbGroups = 2


-- cycle between groups. ignore workspaces past nbGroup * groupSize
switchToGroup direction = do
  x <- logCurrent -- current workspace
  let y = read $ fromJust x -- read converts string -> int

  if direction == "next"
    then do
      if y+groupSize > groupSize * nbGroups then switchWorkspace $ -groupSize * ((y-1) `div` groupSize) else switchWorkspace $ groupSize

  else if direction == "prev"
    then do
      if y-groupSize < 1 then switchWorkspace $ groupSize * (nbGroups - 1) else switchWorkspace $ -groupSize

  else pure ()

-- cycle between groups
switchToGroupIncludingScratch direction = do
  if direction == "next" then switchWorkspace $ groupSize
  else if direction == "prev" then switchWorkspace $ -groupSize
  else pure ()

printWorkspace :: Maybe [Char] -> X()
printWorkspace x = spawn ("dunstify 'Value: " ++ fromJust x ++ "'")
