module Bindings.Windows where
    
import XMonad
-- import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves (rotSlavesUp, rotSlavesDown)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.SwapPromote (swapPromote, swapHybrid)
import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), ChordKeys(AnyKeys))
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowGo (raise, raiseBrowser)
import qualified Data.Map as M -- see XMonad.Doc.Extending
import XMonad.Layout.ResizableTile (MirrorResize(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..))
import XMonad.Layout.Hidden (hideWindow, popOldestHiddenWindow)


window = [ ("M-o", (selectWindow def{cancelKey = xK_Escape}) >>= (`whenJust` windows . W.focusWindow))
         , ("M-S-o", (selectWindow def{
                        emFont = "xft: Cambria-80",
                                 sKeys = AnyKeys [xK_r, xK_e, xK_r, xK_y, xK_u, xK_i],
                                         cancelKey = xK_Escape}) >>= (`whenJust` killWindow))

         -- move focus in stack
         , ("M-m", windows W.focusDown)
         -- move window in stack
         , ("M-S-m", windows W.swapDown)
             
         , ("M-w", sendMessage $ MirrorExpand)
         , ("M-S-w", sendMessage $ MirrorShrink)
           
         , ("M-t", withFocused toggleFloat)
         , ("M-<Return>", whenX (swapHybrid False) promote)

         , ("M-l", rotSlavesUp)
         , ("M-v", rotSlavesDown)
           
         -- , ("M-u", rotUnfocusedUp)
         -- , ("M-i", rotUnfocusedDown)
         , ("M-S-f", sendMessage ToggleLayout)
                          
         -- raise
         , ("M-a", raise (className =? "Emacs"))
         , ("M-g", raise (appName =? "Godot_Engine"))
         , ("M-r", raiseBrowser)
            
         -- hidden
         -- , ("M-y" , withFocused hideWindow)
         -- , ("M-S-y" , popOldestHiddenWindow) -- popHiddenWindow
         ]


toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))                
