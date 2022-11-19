module Bindings where

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows

import XMonad.Actions.Promote
import XMonad.Actions.SwapPromote
import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), ChordKeys(AnyKeys))
import XMonad.Actions.PerWindowKeys

import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll (killOthers)

import qualified Data.Map as M -- see XMonad.Doc.Extending
import qualified XMonad.Actions.FlexibleManipulate as Flex

import XMonad.Layout.ToggleLayouts

import XMonad.Actions.Search
import qualified XMonad.Prompt as P
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
import XMonad.Prompt.Zsh
import XMonad.Prompt.FuzzyMatch

import qualified Module.Submap as SM -- get 17.1 features
import XMonad.Actions.WindowMenu -- not useful but fun
import XMonad.Layout.Hidden

import XMonad.Util.WorkspaceCompare

--
import XMonad.Util.Loggers
import Text.Read
import Data.Char(digitToInt)
import Data.Maybe
import Data.Typeable

-- BSP
-- , ("M-M1-<Left>",    sendMessage $ ExpandTowards L)
-- , ("M-M1-<Right>",   sendMessage $ ShrinkFrom L)
-- , ("M-M1-<Up>",      sendMessage $ ExpandTowards U)
-- , ("M-M1-<Down>",    sendMessage $ ShrinkFrom U)
-- , ("M-M1-C-<Left>",  sendMessage $ ShrinkFrom R)
-- , ("M-M1-C-<Right>", sendMessage $ ExpandTowards R)
-- , ("M-M1-C-<Up>",    sendMessage $ ShrinkFrom D)
-- , ("M-M1-C-<Down>",  sendMessage $ ExpandTowards D)
-- , ("M-s",            sendMessage $ Swap)
-- , ("M-M1-s",         sendMessage $ Rotate)
-- , ("M-S-C-j",        sendMessage $ SplitShift Prev)
-- , ("M-S-C-k",        sendMessage $ SplitShift Next)
  
myKeys conf@(XConfig {modMask = modMask}) =
    -- main shortcuts
    [ ("M-S-e", spawn "nemo")
    , ("M-S-g", windowMenu)
    , ("M-S-<Return>", spawn "[[ $(($RANDOM % 2)) = 1 ]] && alacritty || kitty") -- spawn random terminal
    --, ("C-M-<Return>", spawn "kitty")
    -- , ("M-x", kill)
    , ("M-x", bindFirst [(className =? "Emacs", pure ()), (pure True, kill)]) -- do not kill emacs by mistake with M-x
    , ("M-p", spawn "rofi -show drun")
    , ("M-t", withFocused toggleFloat)

    , ("M-<Return>", whenX (swapHybrid False) promote)
    , ("M-S-k", killOthers)
    , ("M-S-l", spawn "physlock")
    , ("M-S-p", spawn "polybar-msg cmd toggle")

    , ("M-f", sendMessage ToggleLayout)
    , ("M-u", rotUnfocusedUp)
    , ("M-i", rotUnfocusedDown)
    ] ++

    -- -- easy motion (default escape key: return)
    [ ("M-o", (selectWindow def{cancelKey = xK_Escape}) >>= (`whenJust` windows . W.focusWindow))
    , ("M-S-o", (selectWindow def{
                   emFont = "xft: Cambria-80",
                   sKeys = AnyKeys [xK_r, xK_e, xK_r, xK_y, xK_u, xK_i],
                   cancelKey = xK_Escape}) >>= (`whenJust` killWindow))
    ] ++

    -- function keys
    [ ("<XF86AudioMute>", spawn "pamixer -t")
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 10")
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 10")
    , ("<XF86MonBrightnessDown>", spawn "brightnessctl set 5%-")
    , ("<XF86MonBrightnessUp>", spawn "brightnessctl set +5%")
    , ("<Print>", spawn "flameshot screen")
    ] ++

    -- scratchpads
    [
      ("M-e", namedScratchpadAction scratchpads "nemo")
    ] ++

    -- raise
    [ ("M-a", raise (className =? "Emacs"))
    , ("M-z", raiseBrowser)
    ] ++

    -- number workspaces
    [ ("M-" ++ modKey2 ++ [keyChar], windows $ windowOperation workspaceId)
    | (workspaceId, keyChar) <- zip (workspaces conf) "&é\"'(-è_ç"
    , (windowOperation, modKey2) <- [(W.greedyView, ""), (shift'n'view, "S-"), (W.shift, "C-")]                                    
    ] ++ 

    -- grouped workspaces
    [ ("M-<Up>", (switchWorkspace 3))
    , ("M-<Down>", (switchWorkspace (-3)))
    , ("M-<Left>", (switchInGroup "left"))
    , ("M-<Right>", (switchInGroup "right"))
    ] ++

    -- normal workspaces
    [ ("M-S-<Left>", prevWS)
    , ("M-S-<Right>", nextWS)
    , ("M-C-<Left>", shiftToPrev >> prevWS)
    , ("M-C-<Right>", shiftToNext >> nextWS)
    , ("M-<Tab>", toggleWS)
    ] ++

    -- prompts
    [ ("M-s", promptSearch myPromptConfig google)
    , ("M-S-h", manPrompt myPromptConfig)
    , ("M-b", shellPrompt myPromptConfig)

    -- Goto, Bring, BringCopy, BringToMaster, WithWindow String (Window -> X ())
    -- , ("M-S-b", windowPrompt myPromptConfig Bring allWindows) -- { P.autoComplete = Just 500000 }
    , ("M-S-b", windowMultiPrompt myPromptConfig [(Goto, allWindows), (Bring, wsWindows), (Goto, wsWindows)])

    , ("M-n", workspacePrompt myPromptConfig (windows . W.shift))
    , ("M-S-n", xmonadPrompt myPromptConfig)
    , ("M-S-v", zshPrompt myPromptConfig "/etc/profiles/per-user/ubunteous/bin/zsh")

    ] ++

    -- submap
    [ ("M-g", SM.submap . M.fromList $
      [ ((0, xK_t), spawn "alacritty")
      , ((0, xK_y), spawn "kitty")
      ])
    ] ++

    [ ("M-S-c", SM.visualSubmap def $ M.fromList $ map (\(k, s, a) -> ((0, k), (s, a)))
      [ (xK_t, "kritty", spawn "alacritty") -- "M-S-c t" => alacritty
      , (xK_r, "kitty", spawn "kitty") -- "M-S-c r" => kitty
      ])
    ] ++

    -- hidden
    [ ("M-S-d" , withFocused hideWindow)
    , ("M-S-j" , popOldestHiddenWindow) -- popHiddenWindow
    ]



switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . W.greedyView

wsBy :: Int -> X (WorkspaceId)
wsBy = findWorkspace getSortByIndex Next anyWS


-- create groups within which you can shift
switchInGroup direction = do
  x <- logCurrent

  let y = read $ fromJust x -- read converts string -> int
  if direction == "left"
    then do
      if y `mod` 3 /= 1 then prevWS else switchWorkspace 2
   
  else if direction == "right"
    then do
      if y `mod` 3 /= 0 then nextWS else switchWorkspace (-2)

  else pure ()


-- shift window to workspace and follow it
shift'n'view i = W.greedyView i . W.shift i

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))

myBackgroundColor = "#151515"
myContentColor = "#ffa500"
myPromptConfig = def
                 {
                   P.position = P.CenteredAt 0.4 0.5 -- P.Top
                 , P.promptBorderWidth = 0
                 , P.defaultText = ""
                 , P.alwaysHighlight = True
                 , P.historySize = 0
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



myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> Flex.mouseWindow Flex.position w))
    , ((modm, button3), (\w -> focus w >> Flex.mouseWindow Flex.resize w))
      
    ]


scratchpads =
    [  NS "nemo" "nemo" (className =? "Nemo") nonFloating
    ]
