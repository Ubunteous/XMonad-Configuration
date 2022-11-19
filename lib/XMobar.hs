module XMobar where

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers
    
myXmobarPP :: PP
myXmobarPP = def
    { ppCurrent = wrap ("<box type=Bottom width=2 mb=2 color=" ++ "#c678dd" ++ ">") "</box>" -- wrap (blue "[") (blue "]") -- wrap " " "" . xmobarBorder "Top" "#8be9fd" 2
    -- , ppVisible = wrap (blue "[") (blue "]")
    -- , ppVisibleNoWindows = wrap (blue "[") (blue "]") -- maybe

    -- , ppHidden = white . wrap " " ""
    -- , ppHiddenNoWindows = lowWhite . wrap " " ""
    -- , ppUrgent = red . wrap (yellow "!") (yellow "!")

    , ppSep = magenta " • "
    , ppWsSep = red " • "

    -- ,  ppTitle = -- check logtiles
    -- , ppTitleSanitize = xmobarStrip
    -- ,  ppLayout =
    -- ppSort
    --ppOutput

    , ppOrder = \[ws, l, _, wins] -> [wins, ws] -- [l, wins, ws]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . red . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue . ppWindow

    -- | Windows should have *some* title, which should not not exceed a sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 25

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
