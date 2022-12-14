module Bindings.Windows where
    
import XMonad
-- import XMonad.Actions.CycleWindows
import XMonad.Actions.RotSlaves
import XMonad.Actions.Promote
import XMonad.Actions.SwapPromote
import XMonad.Actions.EasyMotion (selectWindow, EasyMotionConfig(..), ChordKeys(AnyKeys))
import qualified XMonad.StackSet as W
import XMonad.Actions.WindowGo
import qualified Data.Map as M -- see XMonad.Doc.Extending
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Hidden

import XMonad.Layout.ResizableTile
      
window = [ ("M-b", sendMessage $ MirrorShrink)
         , ("M-n", sendMessage $ MirrorExpand)


         , ("M-o", (selectWindow def{cancelKey = xK_Escape}) >>= (`whenJust` windows . W.focusWindow))
         , ("M-S-o", (selectWindow def{
                        emFont = "xft: Cambria-80",
                                 sKeys = AnyKeys [xK_r, xK_e, xK_r, xK_y, xK_u, xK_i],
                                         cancelKey = xK_Escape}) >>= (`whenJust` killWindow))
           
         , ("M-t", withFocused toggleFloat)
         , ("M-<Return>", whenX (swapHybrid False) promote)

         , ("M-u", rotSlavesUp)
         , ("M-i", rotSlavesDown)
           
         -- , ("M-u", rotUnfocusedUp)
         -- , ("M-i", rotUnfocusedDown)
         , ("M-f", sendMessage ToggleLayout)
                          
           -- raise
         , ("M-a", raise (className =? "Emacs"))
         , ("M-z", raiseBrowser)
            
           -- hidden
         , ("M-y" , withFocused hideWindow)
         , ("M-S-y" , popOldestHiddenWindow) -- popHiddenWindow
         ]


toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
