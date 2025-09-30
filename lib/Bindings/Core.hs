module Bindings.Core where

import XMonad
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), namedScratchpadAction, nonFloating)
import XMonad.Actions.PerWindowKeys (bindFirst)
import XMonad.Actions.WithAll (killOthers)
import XMonad.Hooks.ManageHelpers (($?))

-- Float
import qualified XMonad.StackSet as W
import XMonad.Hooks.Place (placeFocused, simpleSmart)
-- Mouse
import qualified Data.Map as M -- see XMonad.Doc.Extending
import qualified XMonad.Actions.FlexibleManipulate as Flex

-- import XMonad.Actions.CycleWS (shiftTo, Direction1D(Next), emptyWS)

import XMonad.Hooks.Place (inBounds, underMouse)
    
core :: [(String, X())]
core = [ ("M-e", spawn "nemo")
       , ("M-b", placeFocused $ inBounds (underMouse (0.5, 0.5)))
       -- , ("M-b", spawn "reaper")
       -- , ("M-b", shiftTo Next emptyWS)
         
       , ("M-S-s", spawn "~/.xmonad/lib/Bindings/commands.sh hdmi")
       , ("M-C-S-s", spawn "xrandr --output eDP-1 --auto")
  
       , ("M-S-<Return>", spawn "wezterm || xterm")
       , ("M-j", spawn "wezterm || xterm")
       -- , ("M-S-<Return>", spawn "[[ $(($RANDOM % 2)) = 1 ]] && alacritty || kitty") -- spawn random terminal
       -- , ("M-S-<Return>", spawn "if [ $(shuf -i 0-1 -n 1) = 1 ]; then alacritty; else kitty; fi")
       -- , ("M-S-<Return>", spawn "~/.xmonad/lib/Bindings/commands.sh randTerm")
      
       -- FLoating
       , ("M-C-z", placeFocused simpleSmart)

       -- do not kill emacs/reaper/godot editor by mistake with M-x
       -- <Optional project name> REAPER vx.xx - Registered to <name> (Licensed for personal/small business use)
       , ("M-x", bindFirst [(className =? "Emacs" <||> className =? "Godot" <||> title $? "use)", pure ()), (pure True, kill)])
       , ("M-C-x", kill)
       , ("M-<", spawn "xkbset bell sticky -twokey -latchlock feedback led stickybeep")      
       , ("M-S-k", killOthers)

       -- without this line, M-. changed the layout to
       -- a single column (could also be a masterless tall). Was it a bug ?
       , ("M-.", spawn "rofi -show drun")
       , ("M-y", spawn "rofi -show drun")
       , ("M-p", spawn "rofi -show drun")
       , ("M-!", spawn "rofi -show drun") -- colemak
       -- , ("M-/", spawn "rofi -show drun") -- colemak
       , ("M-$", spawn "~/.config/rofi/powermenu.sh")      
       -- , ("M-C-p", spawn "polybar-msg cmd toggle")
       , ("M-C-p", spawn "~/.xmonad/lib/Bindings/commands.sh toggleBar")
       , ("M-S-p", spawn "~/.xmonad/lib/Bindings/commands.sh toggleBar")
       , ("M-S-l", spawn "~/.xmonad/lib/Bindings/commands.sh lock")
       , ("M-S-i", spawn "~/.xmonad/lib/Bindings/commands.sh lock")

       -- function keys
       -- can be improved by creating a script which shows a different icon if mute toggle leads to 0 or full sound with sound on/off string
       , ("<XF86AudioMute>", spawn "~/.xmonad/lib/Bindings/commands.sh audioMute")
       , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/lib/Bindings/commands.sh audioDown")
       , ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/lib/Bindings/commands.sh audioUp")
       , ("<XF86MonBrightnessDown>", spawn "~/.xmonad/lib/Bindings/commands.sh brightDown")
       , ("<XF86MonBrightnessUp>", spawn "~/.xmonad/lib/Bindings/commands.sh brightUp")
       -- other screen brightness: xrandr --output HDMI-1 --brightness 1

       , ("<Print>", spawn "flameshot screen")
       , ("M-<Print>", spawn "flameshot gui")  
    
       -- xkb-switch -n -- switch to the next xkb layout
       -- , ("M-c", spawn "gkbd-keyboard-display -l 'fr-colemak'") -- shows Colemak rather than current layout
       , ("M-c", spawn "setxkbmap -query | grep 'layout' | cut -d ' ' -f 6 | xargs gkbd-keyboard-display -l")
       , ("M-S-c", spawn "pix ~/Pictures/Screenshots/split.png")
       -- , ("M-*", spawn "if [ $(setxkbmap -query | grep 'layout' | sed 's/^.*:*\\s //') = 'fr' ]; then setxkbmap fr-colemak; eww update layout='Colemak (fr)'; else setxkbmap fr; eww update layout='Azerty (fr)'; fi")
         
       -- screenkey helper
       -- , ("M-S-.", spawn "if pgrep screenkey; then pkill screenkey; dunstify -t 750 'screenkey disabled'; else dunstify -t 750 'screenkey activated' && screenkey -t .5 -s small --no-whitespace --mods-mode emacs; fi")
      
       , ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")

       -- refresh Sine
       -- , ("M-C-o", namedScratchpadAction scratchpads "sine")
       , ("M-C-o", spawn "wmctrl -r 'Sine Player' -t 8 && wmctrl -R 'Sine Player'")

       , ("M-C-e", namedScratchpadAction scratchpads "nemo")
       ]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
   [ ((modm, button1), (\w -> do
                          -- floats <- gets (W.floating . windowset)
                          -- if (w `M.member` floats) then focus w >> do

                          focus w
                          -- make current floating window appear on top
                          windows W.shiftMaster
                          Flex.mouseWindow Flex.position w))
   , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))
   ]


scratchpads = [ NS "nemo" "nemo" (className =? "Nemo") nonFloating ]
              -- , NS "sine" "wine ~/.wine/drive_c/Program\\ Files/SINE\\ Player/SINE\\ Player.exe" (title =? "SINE Player") nonFloating
              -- ]
            
