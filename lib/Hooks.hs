module Hooks where

import XMonad
-- import qualified XMonad.StackSet as W (swapDown)
import XMonad.Hooks.ManageHelpers (isDialog, isFullscreen, doCenterFloat, composeOne, (-?>), doSink, doLower, currentWs, doFullFloat, windowTag, ($?), (^?)) -- ^? prefix, ~? infix, $? suffix, /=? not equal
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(End))

import XMonad.Hooks.RefocusLast (refocusLastLogHook, refocusLastWhen, refocusingIsActive)
import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Hooks.FadeWindows (fadeWindowsLogHook, fadeWindowsEventHook, transparency, isUnfocused, opaque)
-- import XMonad.Hooks.MoreManageHelpers -- does not exist in
   
-- import XMonad.Util.WindowPropertiesRE ((~?)) -- regular expressions

import XMonad.Actions.CycleWS (shiftToNext, nextWS, moveTo, Direction1D(Next), emptyWS, findWorkspace, emptyWS, shiftTo)
import qualified Data.Map as M -- see XMonad.Doc.Extending
import qualified XMonad.StackSet as W
import Control.Monad (liftM2)

import Data.Monoid (Endo(Endo))
import XMonad.Util.Loggers (logCurrent)

import XMonad.Util.WorkspaceCompare (getSortByIndex)

-- import XMonad.Hooks.Place
-- myPlaceHook :: ManageHook
-- myPlaceHook = placeHook simpleSmart
-- myPlaceHook = placeHook $ inBounds (underMouse (0, 0))
-- myPlaceHook = placeHook $ withGaps (16,0,16,0) (smart (0.5,0.5))

-- for other xprop string properties:
--  appName / resource and className are in WM_CLASS(STRING)
--  title is WM_NAME(STRING)
--  stringProperty "WM_WINDOW_ROLE" =? "presentationWidget" --> doFloat -- use either --> or -?>
myManageHook :: ManageHook
myManageHook = composeOne
    [
    -- title =? "WaitingForKey" -?> doIgnore -- window needed but placed in corner
    -- className =? "" -?> doIgnore -- reaper which key (avoid/reduce glitch)
    title ^? "KeySequenceListener" -?> doIgnore  -- reaper which key (avoid mouse move to corner)
    -- , title =? "Matching shortcuts" -?> doFloat                      
    , title =? "Key Sequences" -?> doSink

    -- avoid issues with windows which start as title="" and get title="menu"
    -- this affects goodhertz plugins menus for instance
    , title =? "" <&&> className =? "yabridge-host.exe" -?> doIgnore
    -- , title ~? "*" -?> doIgnore -- regexp

    , isDialog -?> doCenterFloat -- prevent floating dialog from appearing below window
    -- , className ~? ".*" -?> doIgnore
    -- , isDialog --> doF W.shiftMaster <+> doF W.swapDown
      
    -- use this with xprop to test problematic windows
    -- , return True -?> doIgnore
    -- , return True -?> doCenterFloat
                  
    , className =?? ["Gimp-2.10", "Gkbd-keyboard-display" 
                    , "confirm", "file_progress", "download", "error"
                    , "Text-input", "Save-preset"] -?> doCenterFloat -- last two are u-he popups
               
    -- vst/midi reaper pop ups 
    , className =? "REAPER" <&&> title =?? ["Edit MIDI", "Routing Matrix", "Actions", "Browse packages", "REAPER Preferences"] <||> title ^? "FX" -?> doSink <+> doF W.swapDown
    , className =? "REAPER" <&&> title ^?? ["VST", "JS"] -?> doSink <+> doShiftX (findWorkspace getSortByIndex Next emptyWS 1) -- detached fx
    -- , className =? "REAPER" <&&> title ^? "Toolbar" -?> doFloat
    -- , className =? "REAPER" -?> doSink <+> doF W.swapDown
    -- , title =? "Insert Virtual Instrument on New Track..." -?> doF W.swapDown  -- doSink
                   
    , className =? "yabridge-host.exe.so" -?> doIgnore
    , className =? "fl64.exe" -?> doSink

    --  isSuffixOf syntorial.exe, sine player.exe, superior drummer 3.exe
    -- , className $? ".exe" <&&> willFloat -?> doSink

    , className =? "superior drummer 3.exe" <&&> willFloat -?> doIgnore
    , className =? "sine player.exe" <&&> title =? "SINE Player" -?> doCenterFloat
    , className =? "sine player.exe" -?> doIgnore -- for menus
    -- , className $? ".exe" <&&> willFloat -?> doCenterFloat

    , return True -?> insertPosition End Newer -- open new windows at the end. Positions: Master, End, Above, Below
    ] -- where
    -- viewShift = doF . liftM2 (.) W.greedyView W.shift -- workspace name as arg (eg: "9")
    -- unfloat = ask >>= doF . W.sink


myLogHook = refocusLastLogHook <> updatePointer (0.5, 0.5) (0.5, 0.5) <> fadeWindowsLogHook myFadeHook

myHandleEventHook = refocusLastWhen refocusingIsActive <> fadeWindowsEventHook -- handleEventHook def

-- this FadeHook requires Picom and serves to change its settings
-- find a window className with xprop | grep 'CLASS'
-- composeOne stops at first match and requires -?> instead of -->
-- myFadeHook = composeAll
myFadeHook = composeOne
    [ className =?? ["Evince", "Pix"] -?> opaque
    , isUnfocused -?> transparency 0.3
    , isFullscreen -?> opaque -- for videos in firefox
    , return True -?> transparency 0.1 -- default transparency
    ]

-- custom operator to use a list for className
(=??) :: Eq a => Query a -> [a] -> Query Bool
q =?? [] = pure False
q =?? (x:xs) = q =? x <||> (q =?? xs)

-- another custom operator to use a suffix list for className
(^??) :: Eq a => Query [a] -> [[a]] -> Query Bool -- query string ?
q ^?? [] = pure False
q ^?? (x:xs) = q ^? x <||> (q ^?? xs)

-- doShift :: Query WorkspaceId -> ManageHook
-- doShift ws = ask >>= \w -> Endo . (`W.shiftWin` w) <$> ws

-- doView :: Query WorkspaceId -> ManageHook
-- doView ws = Endo . W.greedyView <$> ws

doShiftX :: X WorkspaceId -> ManageHook
doShiftX ws = ask >>= \w -> liftX $ Endo . (`W.shiftWin` w) <$> ws

-- doViewX :: X WorkspaceId -> ManageHook
-- doViewX ws = liftX $ Endo . W.greedyView <$> ws
