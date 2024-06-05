import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Hooks.EwmhDesktops -- ewmh compliance
import XMonad.Hooks.ManageDocks (docks) -- for polybar
import XMonad.Actions.SwapPromote (masterHistoryHook)
import XMonad.Util.EZConfig (additionalKeysP, checkKeymap)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import XMonad.Actions.Navigation2D
import XMonad.Hooks.Modal (modal, noModMode)

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
import Bindings.Modes
import Bindings.Prompts
import Bindings.Windows
import Bindings.Workspaces
import Bindings.Navigation2D

main :: IO ()
main = xmonad
       . modal [noModMode, jobMode]
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
  -- spawnOnce "/home/ubunteous/.config/polybar/launch.sh"
  spawnOnce "eww daemon && eww open bar"

  -- spawnOnce "screenkey -t .5 -s small --no-whitespace --mods-mode emacs"
  -- spawnOnce "picom --experimental-backends"

  spawnOnce "firefox &"
  spawnOnOnce "1" "emacs &"

  spawnOnce "if [ $(xrandr --query | grep -c 'HDMI-1 connected') -eq 1 ]; then xrandr --output eDP-1 --off --output HDMI-1 --auto; fi"
                    
  -- spawnOnce "kmonad $HOME/.nix.d/files/kmonad.kbd &"

  -- spawnOnce "watch -n 7200 dunstify -u critical -t 30000 'It is time for a break'"
  -- spawnOnce "watch -n 65 dunstify -u critical -t 30000 'Break Time'"
  spawnOnce "~/.xmonad/lib/Bindings/commands.sh breakTime" 
            
  -- check if there is an incorrect or duplicate key binding
  return ()
  -- checkKeymap myConfig (myKeys myConfig) -- buggy in v17.2. always triggers error for every binding


myKeys conf@(XConfig {modMask = modMask}) =
    core ++ workspace ++ prompt ++ window ++ bsp_bind ++ nav2d ++ modeKeys ++

    -- numbered workspaces
    [ ("M-" ++ modKey2 ++ [keyChar], windows $ windowOperation workspaceId)
    | (workspaceId, keyChar) <- zip (workspaces conf) "&é\"'(-è_ç"
    , (windowOperation, modKey2) <- [(W.greedyView, ""), (shift'n'view, "S-"), (W.shift, "C-")]]


-- shift window to workspace and follow it
shift'n'view i = W.greedyView i . W.shift i
