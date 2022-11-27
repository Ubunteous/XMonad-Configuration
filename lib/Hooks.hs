module Hooks where

import XMonad    
import qualified XMonad.StackSet as W (swapUp)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))

import XMonad.Hooks.RefocusLast
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.FadeWindows
-- import XMonad.Hooks.MoreManageHelpers -- does not exist in 17.1

-- Make some apps (including gimp) float by default
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp-2.10" --> doCenterFloat -- find name with xprop
    , isDialog --> doF W.swapUp
    , insertPosition End Newer -- open new windows at the end
    -- Options: doFloat vs doCenterFloat
    -- Options for insertPosition: Focus (Newer/Older) and Position (Master, End, Above, Below)
    ]



myLogHook = refocusLastLogHook <> updatePointer (0.5, 0.5) (0.5, 0.5) <> fadeWindowsLogHook myFadeHook

myHandleEventHook = refocusLastWhen refocusingIsActive <> fadeWindowsEventHook -- handleEventHook def

-- this FadeHook requires Picom and serves to change its settings
myFadeHook = composeAll
    [ transparency 0.1 -- default transparency
    , isUnfocused --> transparency 0.3
    -- , (title =? "Alacritty") --> transparency 0.8
    , (className =? "Evince") --> opaque
    ]
    -- [ opaque
