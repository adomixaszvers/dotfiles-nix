import XMonad
import XMonad.Util.EZConfig (mkNamedKeymap)
import XMonad.Util.NamedActions (addDescrKeys, addName, subtitle, xMessage)

main = xmonad
        $ addDescrKeys ((myModMask, xK_F1), xMessage) myAdditionalKeys
        $ myConfig


myConfig = def
    { terminal    = "termite"
    , modMask     = mod4Mask
    , borderWidth = 3
    }

myModMask = mod4Mask

myAdditionalKeys c = (subtitle "Custom Keys":) $ mkNamedKeymap c $
        [ ("M-p", addName "Open DRun menu" $ spawn "rofi -show drun")
        , ("M-S-p", addName "Open Run menu" $ spawn "rofi -show run")
        ]

