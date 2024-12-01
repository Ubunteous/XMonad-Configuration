module Bindings.Modes where
    
import XMonad
import XMonad.Actions.CycleWS (prevWS, nextWS)
import XMonad.Hooks.Modal (mode, Mode, mkKeysEz, setMode, noModModeLabel, exitMode)

-- import qualified Data.Map as M
-- import qualified XMonad.StackSet as W

-- mode for mastering tracks by moving to workspaces quickly
-- (1 plugin per workspace for the mastering chain)
-- masterMode :: Mode
-- masterMode = mode "masterMode" $ mkKeysEz
--   [ ("d", nextWS)
--   , ("s", prevWS)
--   , ("t", withFocused toggleFloat)
--   , ("q", sequence_ [exitMode, spawn "eww update current_mode='Normal'"])
--   ]

jobMode :: Mode
jobMode = mode "jobMode" $ mkKeysEz
  [ ("&", spawn "sleep 0.1 && xdotool type 'link url'")
  , ("é", spawn "sleep 0.1 && xdotool type 'name'")
  -- ("&", spawn "~/.xmonad/lib/Bindings/commands.sh jobget 1'")
  , ("\"", spawn "sleep 0.1 && xdotool type 'surname'")

  , ("'", spawn "sleep 0.1 && xdotool type 'mail'")

  , ("(", spawn "sleep 0.1 && xdotool type 'phone'")
  -- , ("(", spawn "sleep 0.1 && xdotool type 'address'")
  -- , ("-", spawn "sleep 0.1 && xdotool type 'postcode'")

  -- , ("è", spawn "sleep 0.1 && xdotool type  'city'")

  -- -- , ("_", spawn "sleep 0.1 && xdotool type ")

  , (")", spawn "setxkbmap us && dunstify us")
  , ("-", spawn "setxkbmap fr-qwerty && dunstify fr")

  , ("M-S-<Left>", prevWS)
  , ("M-S-<Right>", nextWS)

  , ("/", sequence_ [exitMode, spawn "dunstify 'Back to normal mode'"])
  ]

modeKeys = [ ("M-S-n", setMode noModModeLabel)
           -- , ("M-S-r", sequence_ [setMode "masterMode", spawn "eww update current_mode='Master'"])
           , ("M-C-S-j", sequence_ [setMode "jobMode", spawn "dunstify 'Job Mode On'"])  
           ]

-- toggleFloat w = windows (\s -> if M.member w (W.floating s)
--                             then W.sink w s
--                             else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
