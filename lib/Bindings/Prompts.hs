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
    
prompt = [ ("M-r", promptSearch myPromptConfig google)
         , ("M-g", promptSearch myPromptConfig google)
         , ("M-;", promptSearch myPromptConfig google)

         , ("M-d", SM.visualSubmap def $ M.fromList $ map (\(k, s, a) -> ((0, k), (s, a)))
                     [ (xK_q, "man", manPrompt myPromptConfig)
                     , (xK_s, "shell", shellPrompt myPromptConfig)
                     , (xK_d, "window", windowMultiPrompt myPromptConfig [(Goto, allWindows), (Bring, wsWindows), (Goto, wsWindows)])
                     , (xK_f, "window -> ws", workspacePrompt myPromptConfig (windows . W.shift))
                     , (xK_g, "xmonad", xmonadPrompt myPromptConfig)
                     , (xK_h, "zsh", zshPrompt myPromptConfig "/etc/profiles/per-user/ubunteous/bin/zsh")
                     , (xK_j, "internet", promptSearch myPromptConfig google)

                     -- Goto, Bring, BringCopy, BringToMaster, WithWindow String (Window -> X ())
                     , (xK_k, "bring window", windowPrompt myPromptConfig Bring allWindows) -- { P.autoComplete = Just 500000 }
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
