module Bindings.Core where

import XMonad
import XMonad.Util.NamedScratchpad
import XMonad.Actions.PerWindowKeys (bindFirst)
import XMonad.Actions.WithAll (killOthers)

-- Mouse
import qualified Data.Map as M -- see XMonad.Doc.Extending
import qualified XMonad.Actions.FlexibleManipulate as Flex

core :: [(String, X())]
core =
    [ ("M-e", spawn "nemo")
    , ("M-S-e", namedScratchpadAction scratchpads "nemo")

    -- , ("M-S-<Return>", spawn "kitty")
    -- , ("M-S-<Return>", spawn "[[ $(($RANDOM % 2)) = 1 ]] && alacritty || kitty") -- spawn random terminal

    , ("M-S-<Return>", spawn "if [ $(shuf -i 0-1 -n 1) = 1 ]; then alacritty; else kitty; fi")
    , ("M-!", spawn "xkbset bell sticky -twokey -latchlock feedback led stickybeep")
      
    , ("M-x", bindFirst [(className =? "Emacs", pure ()), (pure True, kill)]) -- do not kill emacs by mistake with M-x
    , ("M-p", spawn "rofi -show drun")

    , ("M-S-k", killOthers)
    , ("M-C-p", spawn "polybar-msg cmd toggle")
    , ("M-S-l", spawn "brightnessctl -s set 5 && i3lock -ueni ~/Pictures/gem_full.png; brightnessctl -r")
    , ("M-$", spawn "~/.config/rofi/powermenu.sh")      

    -- function keys
    , ("<XF86AudioMute>", spawn "pamixer -t")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 10")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 10")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")
    , ("<Print>", spawn "flameshot screen")

    , ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    -- scratchpads
    ]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
   [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.position w))
   , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))      
   ]


scratchpads = [ NS "nemo" "nemo" (className =? "Nemo") nonFloating ]
