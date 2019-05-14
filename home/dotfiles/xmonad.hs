import XMonad
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions (addDescrKeys, addName, subtitle, xMessage)
import XMonad.Hooks.DynamicLog

main =
  xmonad =<< xmobar myConfig

myConfig = addDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
  def
    { terminal = "termite"
    , modMask = mod4Mask
    , borderWidth = 3
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , manageHook = manageDocks <+> manageHook defaultConfig
    }

myModMask = mod4Mask

myAdditionalKeys c =
  let ltKeys =
        [ 0x1b1 -- ą
        , 0x1e8 -- č
        , 0x1ea -- ę
        , 0x3ec -- ė
        , 0x3e7 -- į
        , 0x1b9 -- š
        , 0x3f9 -- ų
        , 0x3fe -- ū
        , 0xafe -- „
        ]
   in (subtitle "Custom Keys" :) $
      mkNamedKeymap
        c
        [ ("M-d", addName "Open DRun menu" $ spawn "rofi -show drun")
        , ("M-S-d", addName "Open Run menu" $ spawn "rofi -show run")
        ] ++
      [ ((m .|. myModMask, key), addName (name ++ i) $ windows $ f i)
      | (i, key) <- zip (XMonad.workspaces c) ltKeys
      , (f, name, m) <-
          [ (W.greedyView, "Open workspace", 0)
          , (W.shift, "Move to workspace", shiftMask)
          ]
      ]
