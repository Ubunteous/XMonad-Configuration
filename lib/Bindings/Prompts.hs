module Bindings.Prompts where

import XMonad
import XMonad.Actions.Search
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import XMonad.Prompt.Zsh
import XMonad.Prompt.FuzzyMatch

import qualified Data.Map as M
import qualified XMonad.Prompt as P
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Submap as SM
    
prompt = [ ("M-h", promptSearch myPromptConfig duckEngine)
         , ("M-f", shellPrompt myPromptConfig)

         , ("M-d", SM.visualSubmap def $ M.fromList $ map (\(k, s, a) -> ((0, k), (s, a)))
                     [ (xK_d, "drop window to ws #", workspacePrompt myPromptConfig (windows . W.shift))
                     , (xK_g, "go to window", windowMultiPrompt myPromptConfig [(Goto, allWindows), (Bring, wsWindows), (Goto, wsWindows)])
                     , (xK_i, "internet", promptSearch myPromptConfig duckEngine) -- google

                     -- Goto, Bring, BringCopy, BringToMaster, WithWindow String (Window -> X ())
                     , (xK_k, "bring window", windowPrompt myPromptConfig Bring allWindows) -- { P.autoComplete = Just 500000 }

                     , (xK_m, "man", manPrompt myPromptConfig)
                     , (xK_n, "nixpkgs search", promptSearch myPromptConfig nixEngine)
                     , (xK_s, "shell", shellPrompt myPromptConfig)
                     , (xK_x, "xmonad", xmonadPrompt myPromptConfig)
                     , (xK_z, "zsh", zshPrompt myPromptConfig "/etc/profiles/per-user/ubunteous/bin/zsh")
                     ])]
         

myBackgroundColor = "#151515"
myContentColor = "#ffa500"
myPromptConfig = def
                 {
                   P.position = P.CenteredAt 0.4 0.5 -- P.Top
                 , P.promptBorderWidth = 0
                 , P.defaultText = ""
                 , P.alwaysHighlight = True
                 , P.historySize = 8
                 , P.height = 64
                 , P.font = "xft:DejaVu Sans Condensed-20:normal"
                 , P.bgColor = myBackgroundColor
                 , P.fgColor = myContentColor
                 , P.bgHLight = myBackgroundColor
                 , P.fgHLight = myContentColor
                 , P.borderColor = myBackgroundColor
                 , P.complCaseSensitivity = P.CaseInSensitive
                 , P.searchPredicate = fuzzyMatch
                 , P.sorter = fuzzySort
                 , P.changeModeKey = xK_twosuperior -- xK_Super_L
                 }

duckEngine = searchEngine "DuckDuckGo" "https://duckduckgo.com/?ks=s&k7=24273a&kae=d&kj=24273a&k9=8bd5ca&kaa=f5a97f&k21=363a4f&k1=-1&kak=-1&kax=-1&kaq=-1&kap=-1&kao=-1&kau=-1&q="

nixEngine = searchEngine "Nix Package"
            "https://search.nixos.org/packages?channel=unstable&from=0&size=50&sort=relevance&type=packages&query="
