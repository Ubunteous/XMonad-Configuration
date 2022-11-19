module Bindings.Navigation2D where
    
import XMonad
import XMonad.Actions.Navigation2D    

nav2d = [ ("M-C-s", switchLayer)
        -- Directional navigation of windows
        , ("M-M1-l", windowGo R True)
        , ("M-M1-h", windowGo L True)
        , ("M-M1-k", windowGo U True)
        , ("M-M1-j", windowGo D True)
          
        -- Swap adjacent windows
        , ("M-C-l", windowSwap R True)
        , ("M-C-h", windowSwap L True)
        , ("M-C-k", windowSwap U True)
        , ("M-C-j", windowSwap D True)
        ]

myNavigation2DConfig = def { defaultTiledNavigation = centerNavigation }
