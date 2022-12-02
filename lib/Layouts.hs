module Layouts where

import XMonad
import XMonad.Layout -- Tall, Full and more
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
-- import XMonad.Layout.Dwindle -- dwindle = Dwindle R CW 1.4 1.1

import XMonad.Layout.Fullscreen (fullscreenFull, fullscreenSupport)
import XMonad.Layout.NoBorders (smartBorders) -- no border if single window
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LimitWindows
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Hidden
import XMonad.Layout.ShowWName
import XMonad.Layout.IfMax

import XMonad.Layout.Renamed

myLayout = custom . hiddenWindows $
           toggleLayouts bsp tallGrid ||| full
    where
      custom x = showWName' swn_config .
                 smartSpacing 3 .
                 smartBorders .
                 avoidStruts $
                 x
                 
      full = Full
      bsp = renamed [Replace "BSP"] emptyBSP
      rTall = limitSelect 1 2 $ ResizableTall 1 (1/20) (1/2) []
      grid = GridRatio (4/3)
      tallGrid = renamed [Replace "Tall Grid"] $ (IfMax 4 rTall grid)

swn_config = def
             { swn_color = "teal"
             , swn_bgcolor = "black"
             , swn_fade = 0.15 }
