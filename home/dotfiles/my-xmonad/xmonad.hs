{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}

import qualified Codec.Binary.UTF8.String      as UTF8
import           Control.Exception              ( bracket )
import qualified DBus                          as D
import qualified DBus.Client                   as D
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( hClose
                                                , hPutStr
                                                )
import           System.Posix.Unistd            ( getSystemID
                                                , nodeName
                                                )
import           XMonad
import           XMonad.Actions.PhysicalScreens ( PhysicalScreen(..)
                                                , viewScreen
                                                , sendToScreen
                                                )
import           XMonad.Hooks.DynamicLog        ( PP(..)
                                                , dynamicLogWithPP
                                                , ppCurrent
                                                , ppHidden
                                                , ppOutput
                                                , ppSep
                                                , ppTitle
                                                , ppUrgent
                                                , ppVisible
                                                , ppWsSep
                                                , shorten
                                                , wrap
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.ManageDocks       ( ToggleStruts(ToggleStruts)
                                                , avoidStruts
                                                , docks
                                                , manageDocks
                                                )
import           XMonad.Hooks.ManageHelpers     ( composeOne
                                                , (-?>)
                                                )
import           XMonad.Hooks.SetWMName         ( setWMName )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( Border(..)
                                                , spacingRaw
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.NamedActions       ( NamedAction(..)
                                                , addDescrKeys'
                                                , addName
                                                , sendMessage'
                                                , separator
                                                , subtitle
                                                , showKm
                                                )
import           XMonad.Util.NamedScratchpad    ( NamedScratchpads
                                                , NamedScratchpad(NS)
                                                , customFloating
                                                , namedScratchpadAction
                                                , namedScratchpadManageHook
                                                , namedScratchpadFilterOutWorkspacePP
                                                )
import           XMonad.Util.Run                ( spawnPipe )
import           XMonad.Util.SpawnOnce          ( spawnOnce )

main :: IO ()
main = do
  host <- fmap nodeName getSystemID
  dbus <- D.connectSession
    -- Request access to the DBus name
  _    <- D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad . hostSpecific host . ewmh . docks $ myConfig
    { logHook = dynamicLogWithPP
                . namedScratchpadFilterOutWorkspacePP
                . myLogHook
                $ dbus
    }

myTerminal :: String
myTerminal = "termite"

myConfig = addDescrKeys'
  ((myModMask, xK_F1), showKeybindings)
  myKeysDescr
  def { terminal           = myTerminal
      , focusedBorderColor = "#8BE9FD"
      , modMask            = mod4Mask
      , borderWidth        = 2
      , handleEventHook    = fullscreenEventHook <+> handleEventHook def
      , layoutHook         = myMainLayout
      , manageHook         = myManageHook
      , startupHook        = myStartupHook
      , workspaces         = myWorkspaces
      }

myModMask :: KeyMask
myModMask = mod4Mask

ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0 :: String
ws1 = "I"
ws2 = "II"
ws3 = "III"
ws4 = "IV"
ws5 = "V"
ws6 = "VI"
ws7 = "VII"
ws8 = "VIII"
ws9 = "IX"
ws0 = "X"

myWorkspaces :: [String]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawnOnce "feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png"
  spawnOnce "systemctl --user restart polybar.service"

myMainLayout = smartBorders . avoidStruts $ tiled ||| Mirror tiled ||| Full
 where
  tiled =
    spacingRaw True
               (Border outer outer outer outer)
               True
               (Border inner inner inner inner)
               True
      $ Tall 1 (3 / 100) (1 / 2)
  outer = 3
  inner = 5

myManageHook :: ManageHook
myManageHook = composeAll
  [ spawnHook
  , manageDocks
  , namedScratchpadManageHook myScratchpads
  , manageHook def
  ]
 where
  spawnHook = composeOne
    [ className =? "Google-chrome" <||> className =? "Firefox" -?> doShift ws1
    , className =? "jetbrains-idea" -?> doShift ws3
    , className =? "rambox" -?> doShift ws4
    , className =? "Steam" <||> className =? "SmartGit" -?> doShift ws5
    , className =? "libreoffice" -?> doShift ws6
    , className =? "Google Play Music Desktop Player" -?> doShift ws0
    ]

myKeysDescr :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeysDescr conf@XConfig { XMonad.modMask = modm } =
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
          , 0xad2 -- “
          ]
  in
    [ subtitle "launching and killing programs"
    , ( (modm, xK_Return)
      , addName "Launch Terminal" $ spawn $ XMonad.terminal conf
      ) -- %! Launch terminal
    , ( (modm, xK_d)
      , addName "Open DRun menu"
        $ spawn "rofi -show combi -combi-modi window,drun"
      )
    , ( (modm .|. shiftMask, xK_d)
      , addName "Open Run menu" $ spawn "rofi -show run -sidebar-mode"
      )
    , ( (modm .|. shiftMask, xK_q)
      , addName "Close the focused window" kill
      ) -- %! Close the focused window
    , subtitle "changing layouts"
    , ( (modm, xK_space)
      , sendMessage' NextLayout
      ) -- %! Rotate through the available layout algorithms
    , ( (modm .|. shiftMask, xK_space)
      , addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf
      ) -- %!  Reset the layouts on the current workspace to default
    , separator
    , ( (modm, xK_n)
      , addName "Refresh" refresh
      ) -- %! Resize viewed windows to the correct size
    , ((modm, xK_b), addName "Toggle struts" $ sendMessage ToggleStruts)
    , subtitle "move focus up or down the window stack"
    , ( (modm, xK_Tab)
      , addName "Focus down" $ windows W.focusDown
      ) -- %! Move focus to the next window
    , ( (modm .|. shiftMask, xK_Tab)
      , addName "Focus up" $ windows W.focusUp
      ) -- %! Move focus to the previous window
    , ( (modm, xK_j)
      , addName "Focus down" $ windows W.focusDown
      ) -- %! Move focus to the next window
    , ( (modm, xK_k)
      , addName "Focus up" $ windows W.focusUp
      ) -- %! Move focus to the previous window
    , ( (modm, xK_m)
      , addName "Focus the master" $ windows W.focusMaster
      ) -- %! Move focus to the master window
    , subtitle "modifying the window order"
    , ( (modm .|. shiftMask, xK_Return)
      , addName "Swap with the master" $ windows W.swapMaster
      ) -- %! Swap the focused window and the master window
    , ( (modm .|. shiftMask, xK_j)
      , addName "Swap down" $ windows W.swapDown
      ) -- %! Swap the focused window with the next window
    , ( (modm .|. shiftMask, xK_k)
      , addName "Swap up" $ windows W.swapUp
      ) -- %! Swap the focused window with the previous window
    , subtitle "resizing the master/slave ratio"
    , ( (modm, xK_h)
      , sendMessage' Shrink
      ) -- %! Shrink the master area
    , ( (modm, xK_l)
      , sendMessage' Expand
      ) -- %! Expand the master area
    , subtitle "floating layer support"
    , ( (modm, xK_t)
      , addName "Push floating to tiled" $ withFocused $ windows . W.sink
      ) -- %! Push window back into tiling
    , subtitle "change the number of windows in the master area"
    , ( (modm, xK_comma)
      , sendMessage' (IncMasterN 1)
      ) -- %! Increment the number of windows in the master area
    , ( (modm, xK_period)
      , sendMessage' (IncMasterN (-1))
      ) -- %! Deincrement the number of windows in the master area
    , subtitle "quit, or restart"
    , ( (modm .|. shiftMask, xK_c)
      , addName "Quit" $ io exitSuccess
      ) -- %! Quit xmonad
    , ((modm, xK_Pause), addName "Power menu" $ spawn "rofi-powermenu")
    , ( (modm, xK_q)
      , addName "Restart" $ spawn "xmonad --recompile && xmonad --restart"
      ) -- %! Restart xmonad
    , subtitle "scratchpads"
    , ( (modm .|. controlMask, xK_e)
      , addName "Emacs scratchpad" $ namedScratchpadAction myScratchpads nsEmacs
      )
    , ( (modm .|. controlMask, xK_s)
      , addName "Terminal scratchpad"
        $ namedScratchpadAction myScratchpads nsTerminal
      )
    ]
    ++ subtitle "switching workspaces"
    :  [ ((m .|. modm, k), addName (n ++ i) $ windows $ f i)
       | (f, m, n) <-
         [ (W.greedyView, 0        , "Switch to workspace ")
         , (W.shift     , shiftMask, "Move client to workspace ")
         ]
       , (i, k) <- concatMap (zip (XMonad.workspaces conf))
                             [[xK_1 .. xK_9] ++ [xK_0], ltKeys]
       ]
    ++ subtitle "switching screens"
    :  [ ((modm .|. mask, key), addName (name ++ show sc) $ f def (P sc))
       | (key, sc)       <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f, mask, name) <-
         [ (viewScreen  , 0        , "View screen ")
         , (sendToScreen, shiftMask, "Move to screen ")
         ]
       ]
    ++ [ subtitle "extra"
       , ((0, xK_Print), addName "Make screenshot" $ spawn "maimpick")
       ]

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ bracket
  (spawnPipe "zenity --text-info --font=\"NotoMono Nerd Font\"")
  hClose
  (\h -> hPutStr h (unlines $ showKm x))

hostSpecific :: String -> XConfig l -> XConfig l
hostSpecific "adomas-jatuzis-nixos" conf@XConfig { startupHook = oldStartupHook }
  = conf { startupHook = oldStartupHook >> spawnOnce "rambox" }
hostSpecific _ conf = conf

myLogHook :: D.Client -> PP
myLogHook dbus = def { ppOutput  = dbusOutput dbus
                     , ppCurrent = wrap "%{B#666a73} " " %{B-}"
                     , ppVisible = wrap "%{B#404552} " " %{B-}"
                     , ppUrgent  = wrap "%{F#da4453} " " %{F-}"
                     , ppHidden  = wrap " " " "
                     , ppWsSep   = ""
                     , ppSep     = " : "
                     , ppTitle   = shorten 100
                     }

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName)
        { D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
  D.emit dbus signal
 where
  objectPath    = D.objectPath_ "/org/xmonad/Log"
  interfaceName = D.interfaceName_ "org.xmonad.Log"
  memberName    = D.memberName_ "Update"

nsEmacs, nsTerminal :: String
nsEmacs = "emacs"
nsTerminal = "terminal"

myScratchpads :: NamedScratchpads
myScratchpads =
  [ NS nsEmacs    "emacs"                          (className =? "Emacs")  hook
  , NS nsTerminal (myTerminal ++ " -t scratchpad") (title =? "scratchpad") hook
  ]
  where hook = customFloating $ W.RationalRect 0.025 0.025 0.95 0.95
