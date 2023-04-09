module Hooks where

import XMonad
import qualified XMonad.StackSet as W (swapDown, floating)
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doCenterFloat)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))

import XMonad.Hooks.RefocusLast
import XMonad.Actions.UpdatePointer

import XMonad.Hooks.FadeWindows
-- import XMonad.Hooks.MoreManageHelpers -- does not exist in 17.1

-- Make some apps (including gimp) float by default
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "Gimp-2.10" --> doCenterFloat -- find name with xprop | grep 'CLASS'
    , className =? "Gkbd-keyboard-display" --> doF W.swapDown
                                
    , isDialog --> doF W.swapDown -- prevent floating dialog from appearing below window
    , className =? "confirm" --> doFloat
    , className =? "file_progress" --> doFloat
    , className =? "download" --> doFloat
    , className =? "error" --> doFloat
    -- , isFullscreen -->  doFullFloat

    , insertPosition End Newer -- open new windows at the end
                   
    -- Options: doFloat vs doCenterFloat
    -- Options for insertPosition: Focus (Newer/Older) and Position (Master, End, Above, Below)
    ]

myLogHook = refocusLastLogHook <> updatePointer (0.5, 0.5) (0.5, 0.5) <> fadeWindowsLogHook myFadeHook

myHandleEventHook = refocusLastWhen refocusingIsActive <> fadeWindowsEventHook -- handleEventHook def

-- this FadeHook requires Picom and serves to change its settings
-- find a window className with xprop | grep 'CLASS'
myFadeHook = composeAll
    [ transparency 0.1 -- default transparency
    , isUnfocused --> transparency 0.3
    -- , (className =? "i3lock") --> opaque -- does not work
    -- , (title =? "Alacrity's") --> transparency 0.8
    -- , isFullscreen --> opaque
    , (className =? "Evince") --> opaque
    , (className =? "Pix") --> opaque
    ]
