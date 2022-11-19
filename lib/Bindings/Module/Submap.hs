module Bindings.Module.Submap (
                             submap,
                             visualSubmap,
                             submapDefault,
                             submapDefaultWithKey,
                             subName,
                            ) where
import Data.Bits
import qualified Data.Map as M
import XMonad hiding (keys)
import Bindings.Module.Prelude (fix, fromMaybe, keyToString, cleanKeyMask)
import Bindings.Module.XUtils

submap :: M.Map (KeyMask, KeySym) (X ()) -> X ()
submap = submapDefault (return ())

visualSubmap wc keys =
    withSimpleWindow wc descriptions waitForKeyPress >>= \(m', s) ->
        maybe (pure ()) snd (M.lookup (m', s) keys)
  where
    descriptions :: [String]
    descriptions =
        zipWith (\key desc -> keyToString key <> ": " <> desc)
                (M.keys keys)
                (map fst (M.elems keys))

-- | Give a name to an action.
subName :: String -> X () -> (String, X ())
subName = (,)

-- | Like 'submap', but executes a default action if the key did not match.
submapDefault :: X () -> M.Map (KeyMask, KeySym) (X ()) -> X ()
submapDefault = submapDefaultWithKey . const

-- | Like 'submapDefault', but sends the unmatched key to the default
-- action as argument.
submapDefaultWithKey :: ((KeyMask, KeySym) -> X ())
                     -> M.Map (KeyMask, KeySym) (X ())
                     -> X ()
submapDefaultWithKey defAction keys = waitForKeyPress >>=
    \(m', s) -> fromMaybe (defAction (m', s)) (M.lookup (m', s) keys)

-----------------------------------------------------------------------
-- Internal stuff

waitForKeyPress :: X (KeyMask, KeySym)
waitForKeyPress = do
    XConf{ theRoot = root, display = dpy } <- ask

    io $ do grabKeyboard dpy root False grabModeAsync grabModeAsync currentTime
            grabPointer dpy root False buttonPressMask grabModeAsync grabModeAsync
                        none none currentTime

    (m, s) <- io $ allocaXEvent $ \p -> fix $ \nextkey -> do
        maskEvent dpy (keyPressMask .|. buttonPressMask) p
        ev <- getEvent p
        case ev of
          KeyEvent { ev_keycode = code, ev_state = m } -> do
            keysym <- keycodeToKeysym dpy code 0
            if isModifierKey keysym
                then nextkey
                else return (m, keysym)
          _ -> return (0, 0)
    m' <- cleanKeyMask <*> pure m
    io $ do ungrabPointer dpy currentTime
            ungrabKeyboard dpy currentTime
            sync dpy False
    pure (m', s)
