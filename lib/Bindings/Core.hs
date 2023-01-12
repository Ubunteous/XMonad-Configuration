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
    , ("M-<", spawn "xkbset bell sticky -twokey -latchlock feedback led stickybeep")
      
    , ("M-x", bindFirst [(className =? "Emacs", pure ()), (pure True, kill)]) -- do not kill emacs by mistake with M-x
    , ("M-p", spawn "rofi -show drun")

    , ("M-S-k", killOthers)
    , ("M-C-p", spawn "polybar-msg cmd toggle")
    , ("M-S-l", spawn "brightnessctl -s set 5 && i3lock -ueni ~/Pictures/gem_full.png; brightnessctl -r")
    , ("M-$", spawn "~/.config/rofi/powermenu.sh")      

    -- function keys
    -- can be improved by creating a script which shows a different icon if mute toggle leads to 0 or full sound with sound on/off string
    , ("<XF86AudioMute>", spawn "pamixer -t && dunstify -t 750 -h string:x-dunst-stack-tag:'Volume' -h int:value:$(pamixer --get-volume-human) 'Sound'")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 10 && dunstify -t 750 -h string:x-dunst-stack-tag:'Volume' -h int:value:$(pamixer --get-volume-human) 'Sound'")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 10 && dunstify -t 750 -h string:x-dunst-stack-tag:'Volume' -h int:value:$(pamixer --get-volume-human) 'Sound'")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%- && dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' -h int:value:$(brightnessctl -m -d amdgpu_bl0 | awk -F, '{print substr($4, 0, length($4)-1)}' | tr -d '%') 'Brightness'")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5% && dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' -h int:value:$(brightnessctl -m -d amdgpu_bl0 | awk -F, '{print substr($4, 0, length($4)-1)}' | tr -d '%') 'Brightness'")
    , ("<Print>", spawn "flameshot screen")

      -- brightnessctl set +5%
      -- dunstify -t 750 -u low -h string:x-dunst-stack-tag:'Brightness' -h int:value:$(brightnessctl -m -d amdgpu_bl0 | awk -F, '{print substr($4, 0, length($4)-1)}' | tr -d '%') 'Bright'

    , ("M-q", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
    -- scratchpads
    ]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
   [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.position w))
   , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))      
   ]


scratchpads = [ NS "nemo" "nemo" (className =? "Nemo") nonFloating ]
