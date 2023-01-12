import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops -- ewmh compliance
import XMonad.Hooks.ManageDocks (docks) -- for polybar
import XMonad.Actions.SwapPromote (masterHistoryHook)
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import XMonad.Actions.Navigation2D

-- xmobar
-- import XMobar
-- import XMonad.Hooks.StatusBar
-- import XMonad.Hooks.DynamicLog
-- import XMonad.Hooks.StatusBar.PP

-- custom modules
import Layouts
import Hooks
import Bindings.BSP
import Bindings.Core
import Bindings.Prompts
import Bindings.Windows
import Bindings.Workspaces
import Bindings.Navigation2D
    
main :: IO ()
main = xmonad
       . ewmhFullscreen
       . ewmh
       . docks -- for polybar
       . withNavigation2DConfig myNavigation2DConfig
       -- . withEasySB (statusBarProp "xmobar ~/.xmonad/xmobar/xmobarrc" (pure myXmobarPP)) defToggleStrutsKey
       $ myConfig
    where
      toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
      toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

                                               
myConfig = def
  { terminal = "alacritty"
  --, normalBorderColor = "black"
  , focusedBorderColor = "orange"
  , layoutHook = myLayout -- Use custom layouts
  , manageHook = myManageHook <> namedScratchpadManageHook scratchpads -- match certain windows
  , handleEventHook = myHandleEventHook
  , workspaces = map show [1 .. 9 :: Int] -- default
  , modMask = mod4Mask -- Super key
  , mouseBindings = myMouseBindings
  , logHook = myLogHook >> masterHistoryHook
  , startupHook = myStartupHook
  , focusFollowsMouse = True
  } `additionalKeysP` (myKeys myConfig)

   
myStartupHook = do
  -- start polybar. use spawn rather than spawnOnce to solve alsa problem
  -- spawn "/home/ubunteous/.config/polybar/launch.sh"
  spawnOnce "/home/ubunteous/.config/polybar/launch.sh"
  spawnOnce "picom --experimental-backends"
  spawnOnOnce "1" "emacs &"
  spawnOnce "firefox &"

  -- check if there is an incorrect or duplicate key binding
  return ()
  checkKeymap myConfig (myKeys myConfig)


myKeys conf@(XConfig {modMask = modMask}) =
    core ++ workspace ++ prompt ++ window ++ bsp_bind ++ nav2d ++

    -- numbered workspaces
    [ ("M-" ++ modKey2 ++ [keyChar], windows $ windowOperation workspaceId)
    | (workspaceId, keyChar) <- zip (workspaces conf) "&é\"'(-è_ç"
    , (windowOperation, modKey2) <- [(W.greedyView, ""), (shift'n'view, "S-"), (W.shift, "C-")]]

                                            
