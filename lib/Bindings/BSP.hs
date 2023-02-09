module Bindings.BSP where

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Layout.BinarySpacePartition

import qualified XMonad.StackSet as W

-- Get the name of the active layout.
getActiveLayoutDescription :: X String
getActiveLayoutDescription = do
  workspaces <- gets windowset
  return $ description . W.layout . W.workspace . W.current $ workspaces

         
expand direction = do
   layout <- getActiveLayoutDescription

   if layout == "Spacing Hidden BSP"

   then case direction of
          "left" -> sendMessage $ ExpandTowards R
          "right" -> sendMessage $ ExpandTowards L
          _ -> pure ()
                              
   else case direction of
          "left" -> sendMessage Expand
          "right" -> sendMessage Shrink
          _ -> pure ()


bsp_bind = [ ("M-M1-<Left>", sendMessage $ ExpandTowards L)
           , ("M-M1-<Right>", sendMessage $ ExpandTowards R)
           , ("M-M1-<Up>", sendMessage $ ExpandTowards U)
           , ("M-M1-<Down>", sendMessage $ ExpandTowards D)
           -- , ("M-l", sendMessage $ ExpandTowardsBy R 0.025)
           -- , ("M-h", sendMessage $ ExpandTowardsBy L 0.025)

           , ("M-l", expand "left")
           , ("M-h", expand "right")
             
           , ("M-M1-C-<Left>", sendMessage $ ShrinkFrom L)
           , ("M-M1-C-<Right>", sendMessage $ ShrinkFrom R)
           , ("M-M1-C-<Up>", sendMessage $ ShrinkFrom U)
           , ("M-M1-C-<Down>", sendMessage $ ExpandTowards D)

           , ("M-s", sendMessage $ Swap)
           -- , ("M-r", sendMessage $ Rotate)
           , ("M-M1-c", sendMessage $ SplitShift Prev)
           , ("M-M1-v", sendMessage $ SplitShift Next)

           -- , ("M-b", sendMessage $ Balance)
           ]
