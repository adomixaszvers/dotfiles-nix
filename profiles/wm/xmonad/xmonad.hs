{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}

import qualified Colors as C
import Control.Exception (bracket)
import qualified DBus.Client as D
import Graphics.X11.ExtraTypes
import System.Exit (exitSuccess)
import System.IO
  ( hClose,
    hPutStr,
  )
import XMonad
import XMonad.Actions.PhysicalScreens
  ( PhysicalScreen (..),
    sendToScreen,
    viewScreen,
  )
import qualified XMonad.DBus as XD
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    ewmhFullscreen,
  )
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (ToggleStruts),
    checkDock,
  )
import XMonad.Hooks.ManageHelpers
  ( composeOne,
    doCenterFloat,
    isDialog,
    (-?>),
  )
import XMonad.Hooks.RefocusLast (isFloat, refocusLastLayoutHook, refocusLastWhen)
import XMonad.Hooks.Rescreen (addAfterRescreenHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicEasySBs, sbCleanupHook, sbLogHook, sbStartupHook)
import XMonad.Hooks.StatusBar.PP
  ( PP (..),
    dynamicLogString,
    filterOutWsPP,
    ppCurrent,
    ppHidden,
    ppSep,
    ppUrgent,
    ppVisible,
    ppWsSep,
    wrap,
  )
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle1)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed
  ( Rename (..),
    renamed,
  )
import XMonad.Layout.Spacing
  ( Border (..),
    spacingRaw,
  )
import XMonad.Layout.TrackFloating (trackFloating)
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Util.Loggers (Logger, logCurrentOnScreen, logLayoutOnScreen, logTitleOnScreen, shortenL, wrapL)
import XMonad.Util.NamedActions
  ( NamedAction (..),
    addDescrKeys',
    addName,
    sendMessage',
    separator,
    showKm,
    subtitle,
  )
import XMonad.Util.NamedScratchpad
  ( NamedScratchpad (NS),
    NamedScratchpads,
    namedScratchpadAction,
    namedScratchpadManageHook,
    scratchpadWorkspaceTag,
  )
import XMonad.Util.Run
  ( spawnPipe,
  )
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.WorkspaceCompare (getSortByXineramaPhysicalRule)

main :: IO ()
main = do
  dbus <- XD.connect
  _ <- XD.requestAccess dbus
  xmonad . ewmhFullscreen . ewmh . addAfterRescreenHook (restartEww >> spawnFeh) . dynamicEasySBs (myDynamicStatusBar dbus) $ myConfig
  where
    dynamicHook = dynamicPropertyChange "WM_CLASS" (className =? "Spotify" --> doShift ws0)
    myConfig =
      addDescrKeys'
        ((myModMask, xK_F1), showKeybindings)
        myKeysDescr
        def
          { terminal = myTerminal,
            normalBorderColor = C.white,
            focusedBorderColor = C.cyan,
            modMask = mod4Mask,
            borderWidth = 2,
            handleEventHook = mconcat [handleEventHook def, refocusLastWhen isFloat, dynamicHook],
            layoutHook = myLayoutHook,
            manageHook = myManageHook,
            startupHook = myStartupHook,
            workspaces = myWorkspaces
          }
    tiled =
      spacingRaw
        True
        (Border outer outer outer outer)
        True
        (Border inner inner inner inner)
        True
        $ Tall 1 (3 / 100) (1 / 2)
    outer = 3
    inner = 5
    tall = renamed [Replace "Tall"] tiled
    wide = renamed [Replace "Wide"] (Mirror tiled)
    myMainLayout = onWorkspace ws3 (Full ||| tall ||| wide) (mkToggle1 FULL $ tall ||| wide)
    myLayoutHook = smartBorders . refocusLastLayoutHook . trackFloating $ myMainLayout

myTerminal :: String
myTerminal = "kitty"

myModMask :: KeyMask
myModMask = mod4Mask

ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0 :: String
ws1 = "1"
ws2 = "2"
ws3 = "3"
ws4 = "4"
ws5 = "5"
ws6 = "6"
ws7 = "7"
ws8 = "8"
ws9 = "9"
ws0 = "10"

myWorkspaces :: [String]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawnFeh
  spawnOnce "eww daemon"

myDynamicStatusBar :: D.Client -> ScreenId -> IO StatusBarConfig
myDynamicStatusBar dbus sc@(S i) = pure . dbusStatusBarConfig sc dbus $ ppOn i
  where
    ppOn 0 = pure mainPP
    ppOn _ = pure . secondaryPP $ sc

restartEww :: MonadIO m => m ()
restartEww = spawn "eww reload"

spawnFeh :: MonadIO m => m ()
spawnFeh = spawn "feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ spawnHook,
      className =? "lxqt-openssh-askpass" --> doCenterFloat,
      namedScratchpadManageHook myScratchpads,
      manageHook def
    ]
  where
    spawnHook =
      composeOne
        [ isDialog -?> doFloat,
          className =? "trayer" -?> doIgnore,
          className =? "Vampire_Survivors" -?> doFloat,
          checkDock -?> lowerDock,
          className =? "Google-chrome" <||> className =? "firefox" -?> doShift ws1,
          className =? "jetbrains-idea" -?> doShift ws3,
          className =? "Rambox" <||> className =? "discord" -?> doShift ws4,
          className =? "Steam" <||> className =? "SmartGit" <||> className =? "Microsoft Teams - Preview" -?> doShift ws5,
          className =? "libreoffice" -?> doShift ws6,
          className =? "Eclipse" -?> doShift ws7,
          className =? "org.remmina.Remmina" -?> doShift ws8,
          className =? "KeePass2" <||> className =? "KeePassXC" -?> doShift ws9,
          className =? "Google Play Music Desktop Player" -?> doShift ws0
        ]

myKeysDescr :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeysDescr conf@XConfig {XMonad.modMask = modm} =
  let ltKeys =
        [ xK_aogonek, -- ą
          xK_ccaron, -- č
          xK_eogonek, -- ę
          xK_eabovedot, -- ė
          xK_iogonek, -- į
          xK_scaron, -- š
          xK_uogonek, -- ų
          xK_umacron, -- ū
          0xafe, -- „
          0xad2 -- “
        ]
      volumeCommand, brightnessCommand :: String -> X ()
      volumeCommand cmd = spawn $ cmd ++ "&& dunstify -i audio-card -t 2000 -h string:x-dunst-stack-tag:volume \"Volume $(pamixer --get-volume)\""
      brightnessCommand cmd = spawn $ cmd ++ "&& dunstify -i video-display -t 2000 -h string:x-dunst-stack-tag:brightness \"Brightness $(brightnessctl -m| cut -f4 -d,|tr -d %)\""
   in [ subtitle "launching and killing programs",
        ( (modm, xK_Return),
          addName "Launch Terminal" $ spawn $ XMonad.terminal conf
        ),
        ( (modm, xK_d),
          addName "Open DRun menu" $
            spawn "rofi -show combi -combi-modi window,drun,run -show-icons"
        ),
        ( (modm .|. shiftMask, xK_d),
          addName "Open Run menu" $ spawn "rofi -show run -sidebar-mode"
        ),
        ((modm .|. shiftMask, xK_q), addName "Close the focused window" kill),
        ((modm .|. controlMask, xK_q), addName "Kill the focused window" killByPid),
        subtitle "changing layouts",
        ((modm, xK_space), sendMessage' NextLayout),
        ( (modm .|. shiftMask, xK_space),
          addName "Reset the layout" $ setLayout $ XMonad.layoutHook conf
        ),
        separator,
        ((modm, xK_n), addName "Refresh" refresh),
        ((modm, xK_b), addName "Toggle struts" $ sendMessage ToggleStruts),
        ((modm, xK_f), addName "Toggle fullscreen" $ sendMessage (Toggle FULL)),
        subtitle "move focus up or down the window stack",
        ((modm, xK_Tab), addName "Focus down" $ windows W.focusDown),
        ((modm .|. shiftMask, xK_Tab), addName "Focus up" $ windows W.focusUp),
        ((modm, xK_c), addName "Focus down" $ windows W.focusDown),
        ((modm, xK_j), addName "Focus down" $ windows W.focusDown),
        ((modm, xK_k), addName "Focus up" $ windows W.focusUp),
        ((modm, xK_m), addName "Focus the master" $ windows W.focusMaster),
        subtitle "modifying the window order",
        ( (modm .|. shiftMask, xK_Return),
          addName "Swap with the master" $ windows W.swapMaster
        ),
        ((modm .|. shiftMask, xK_j), addName "Swap down" $ windows W.swapDown),
        ((modm .|. shiftMask, xK_k), addName "Swap up" $ windows W.swapUp),
        subtitle "resizing the master/slave ratio",
        ((modm, xK_h), sendMessage' Shrink),
        ((modm, xK_l), sendMessage' Expand),
        subtitle "floating layer support",
        ( (modm, xK_t),
          addName "Push floating to tiled" $ withFocused $ windows . W.sink
        ),
        subtitle "change the number of windows in the master area",
        ((modm, xK_comma), sendMessage' (IncMasterN 1)),
        ((modm, xK_period), sendMessage' (IncMasterN (-1))),
        subtitle "quit, or restart",
        ((modm .|. shiftMask, xK_c), addName "Quit" $ io exitSuccess),
        ((modm, xK_q), addName "XMonad restart" $ spawn "xmonad --restart"),
        ((modm, xK_F4), addName "Power menu" $ spawn "rofi-powermenu"),
        subtitle "scratchpads",
        ( (modm .|. controlMask, xK_e),
          addName "Emacs scratchpad" $ namedScratchpadAction myScratchpads nsEmacs
        ),
        ( (modm .|. controlMask, xK_s),
          addName "Terminal scratchpad" $
            namedScratchpadAction myScratchpads nsTerminal
        ),
        subtitle "dunst",
        ((modm, xK_F9), addName "Close notification" $ spawn "dunstctl close"),
        ((modm .|. shiftMask, xK_F9), addName "Close all notifications" $ spawn "dunstctl close-all"),
        ((modm, xK_F10), addName "Notification history pop" $ spawn "dunstctl history-pop"),
        ((modm, xK_F11), addName "Notification context actions" $ spawn "dunstctl context")
      ]
        ++ subtitle "music player controls" :
      [ ( (modm, xK_F5),
          addName "Player play-pause" $ spawn "playerctl play-pause"
        ),
        ( (0, xF86XK_AudioPlay),
          addName "Player play-pause" $ spawn "playerctl play-pause"
        ),
        ((modm, xK_F6), addName "Player previous" $ spawn "playerctl previous"),
        ((0, xF86XK_AudioPrev), addName "Player previous" $ spawn "playerctl previous"),
        ((modm, xK_F7), addName "Player next" $ spawn "playerctl next"),
        ((0, xF86XK_AudioNext), addName "Player next" $ spawn "playerctl next")
      ]
        ++ subtitle "sound controls" :
      [ ((modm, xK_minus), addName "Decrease volume" $ volumeCommand "pamixer -d 5"),
        ((0, xF86XK_AudioLowerVolume), addName "Decrease volume" $ volumeCommand "pamixer -d 5"),
        ((modm, 0x1be), addName "Increase volume" $ volumeCommand "pamixer -i 5"),
        ((modm, xK_equal), addName "Increase volume" $ volumeCommand "pamixer -i 5"),
        ((0, xF86XK_AudioMute), addName "Toggle mute volume" $ volumeCommand "pamixer -t"),
        ((0, xF86XK_AudioRaiseVolume), addName "Increase volume" $ volumeCommand "pamixer -i 5")
      ]
        ++ subtitle "brightness controls" :
      [ ((0, xF86XK_MonBrightnessUp), addName "Increase brightness" $ brightnessCommand "brightnessctl set +5%"),
        ((0, xF86XK_MonBrightnessDown), addName "Decrease brightness" $ brightnessCommand "brightnessctl set 5%-")
      ]
        ++ subtitle "switching workspaces" :
      [ ((modm .|. mask, k), addName (name ++ i) $ windows $ f i)
        | (f, mask, name) <-
            [ (W.greedyView, noModMask, "Switch to workspace "),
              (W.shift, shiftMask, "Move client to workspace ")
            ],
          (i, k) <-
            concatMap
              (zip (XMonad.workspaces conf))
              [[xK_1 .. xK_9] ++ [xK_0], ltKeys]
      ]
        ++ subtitle "switching screens" :
      [ ((modm .|. mask, key), addName (name ++ show sc) $ f def (P sc))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..],
          (f, mask, name) <-
            [ (viewScreen, noModMask, "View screen "),
              (sendToScreen, shiftMask, "Move to screen ")
            ]
      ]
        ++ [ subtitle "extra",
             ((modm, xK_F2), addName "Run autorandr --change" $ spawn "autorandr --change"),
             ((0, xK_Print), addName "Make screenshot" $ spawn "maimpick")
           ]

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x =
  addName "Show Keybindings" $
    io $
      bracket
        (spawnPipe "zenity --text-info --font=\"NotoMono Nerd Font\"")
        hClose
        (\h -> hPutStr h (unlines $ showKm x))

myLogTitleOnScreen :: ScreenId -> Logger
myLogTitleOnScreen n = do
  c <- withWindowSet $ return . W.screen . W.current
  let f = if n == c then id else wrapL "(" ""
  f . wrapL ": " "" . shortenL 50 . logTitleOnScreen $ n

mainPP :: PP
mainPP =
  filterOutWsPP [scratchpadWorkspaceTag] $
    def
      { ppCurrent = wrap "[" "]",
        ppVisible = wrap "|" "|",
        ppUrgent = wrap "{" "}",
        ppHidden = wrap "" "",
        ppWsSep = " ",
        ppSep = " ",
        ppSort = getSortByXineramaPhysicalRule def,
        ppExtras =
          [ logLayoutOnScreen 0,
            myLogTitleOnScreen 0
          ],
        ppOrder = \case
          (ws : _layout : _title : extras) -> ws : extras
          x -> x
      }

secondaryPP :: ScreenId -> PP
secondaryPP s =
  def
    { ppOrder = \case
        (_ws : _layout : _title : extras) -> extras
        x -> x,
      ppSep = " ",
      ppExtras =
        [ logCurrentOnScreen s,
          logLayoutOnScreen s,
          myLogTitleOnScreen s
        ]
    }

nsEmacs, nsTerminal :: String
nsEmacs = "emacs"
nsTerminal = "terminal"

myScratchpads :: NamedScratchpads
myScratchpads =
  [ NS
      nsEmacs
      "emacs -T scratchpad --fullscreen"
      (title =? "scratchpad" <&&> className =? "Emacs")
      doFloat,
    NS
      nsTerminal
      "kitty --name scratchpad --start-as fullscreen"
      (appName =? "scratchpad" <&&> className =? "kitty")
      doFloat
  ]

-- | Restack dock under lowest managed window.
lowerDock :: ManageHook
lowerDock =
  do
    w <- ask
    mlw <- liftX findLowest
    case mlw of
      Just lw -> liftX $ do
        d <- asks display
        liftIO $ restackWindows d [lw, w]
        return idHook
      Nothing -> return idHook

-- | Find lowest managed window.
findLowest :: X (Maybe Window)
findLowest = withWindowSet $ \ws -> do
  d <- asks display
  r <- asks theRoot
  (_, _, ts) <- liftIO $ queryTree d r
  return (find (`W.member` ws) ts)

dbusStatusBarConfig :: ScreenId -> D.Client -> X PP -> StatusBarConfig
dbusStatusBarConfig (S i) dbus xpp =
  def
    { sbLogHook = io . XD.sendToPath dbus (show i) =<< dynamicLogString =<< xpp,
      sbStartupHook = spawn $ "eww open bar" ++ show i,
      sbCleanupHook = spawn $ "eww close bar" ++ show i
    }

killByPid :: X ()
killByPid = do
  d <- asks display
  mw <- W.peek <$> gets windowset
  whenJust mw (io . void . killClient d)
