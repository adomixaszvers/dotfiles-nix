{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import           XMonad
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                , manageDocks
                                                , ToggleStruts(ToggleStruts)
                                                )
import           XMonad.Hooks.ManageHelpers     ( composeOne
                                                , (-?>)
                                                )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , spacingRaw
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig           ( mkNamedKeymap
                                                , removeKeysP
                                                )
import           XMonad.Util.NamedActions       ( addDescrKeys
                                                , addName
                                                , subtitle
                                                , xMessage
                                                )
main = xmonad . ewmh . docks $ myConfig

myTerminal = "termite"

myConfig =
  addDescrKeys
      ((myModMask, xK_F1), xMessage)
      myAdditionalKeys
      def { terminal           = myTerminal
          , focusedBorderColor = "#8BE9FD"
          , modMask            = mod4Mask
          , borderWidth        = 2
          , handleEventHook    = fullscreenEventHook <+> handleEventHook def
          , layoutHook         = myMainLayout
          , manageHook         = manageDocks <+> myManageHook <+> manageHook def
          , startupHook        = myStartupHook
          }
    `removeKeysP` ["M-p", "M-S-p"]

myModMask = mod4Mask

myStartupHook = do
  spawn "feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png"
  spawn "systemctl --user restart polybar.service"

myMainLayout = smartBorders . avoidStruts $ tiled ||| Mirror tiled ||| Full
 where
  tiled =
    smartBorders
      $ spacingRaw False
                   (Border outer outer outer outer)
                   True
                   (Border inner inner inner inner)
                   True
      $ Tall 1 (3 / 100) (1 / 2)
  outer = 3
  inner = 5

myManageHook = composeOne
  [ className =? "Google-chrome" <||> className =? "Firefox" -?> doShift "1"
  , className =? "jetbrains-idea" -?> doShift "3"
  , className =? "rambox" -?> doShift "4"
  , className =? "Steam" <||> className =? "SmartGit" -?> doShift "5"
  , className =? "libreoffice" -?> doShift "6"
  , className =? "Emacs" -?> doShift "7"
  , className =? "google play music desktop player" -?> doShift "9"
  ]

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
  in
    (subtitle "Custom Keys" :)
    $  mkNamedKeymap
         c
         [ ("M-b", addName "Toggle struts" $ sendMessage ToggleStruts)
         , ( "M-d"
           , addName "Open DRun menu"
             $ spawn "rofi -show combi -combi-modi window,drun"
           )
         , ( "M-S-d"
           , addName "Open Run menu" $ spawn "rofi -show run -sidebar-mode"
           )
         , ("M-S-<Return>", addName "Swap master" $ windows W.swapMaster)
         , ("M-<Return>"  , addName "Spawn terminal" $ spawn myTerminal)
         , ("M-S-q"       , addName "Kill client" kill)
         , ("M-<Pause>"   , addName "Power menu" $ spawn "rofi-powermenu")
         ]
    ++ [ ((m .|. myModMask, key), addName (name ++ " " ++ i) $ windows $ f i)
       | (i, key) <- zip (XMonad.workspaces c) ltKeys
       , (f, name, m) <-
         [ (W.greedyView, "Open workspace"   , 0)
         , (W.shift     , "Move to workspace", shiftMask)
         ]
       ]
