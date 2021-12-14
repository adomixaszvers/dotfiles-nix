{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}

import Control.Exception (bracket)
import Data.List (elemIndex)
import Graphics.X11.ExtraTypes
import Network.HostName (getHostName)
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
import XMonad.Hooks.DynamicProperty (dynamicPropertyChange)
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    ewmhFullscreen,
  )
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (ToggleStruts),
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
import XMonad.Hooks.StatusBar (StatusBarConfig, dynamicEasySBs, statusBarPipe)
import XMonad.Hooks.StatusBar.PP
  ( PP (..),
    filterOutWsPP,
    ppCurrent,
    ppHidden,
    ppSep,
    ppTitle,
    ppUrgent,
    ppVisible,
    ppWsSep,
    shorten,
    wrap,
  )
import XMonad.Layout.MultiToggle (Toggle (..), mkToggle, single)
import XMonad.Layout.MultiToggle.Instances (StdTransformers (FULL))
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed
  ( Rename (..),
    renamed,
  )
import XMonad.Layout.Spacing
  ( Border (..),
    spacingRaw,
  )
import XMonad.Layout.TrackFloating (trackFloating)
import qualified XMonad.StackSet as W
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

main :: IO ()
main = xmonad . ewmhFullscreen . ewmh . addAfterRescreenHook restartPolybar . dynamicEasySBs myDynamicStatusBar $ myConfig
  where
    dynamicHook = dynamicPropertyChange "WM_NAME" (className =? "Spotify" --> doShift ws0)
    myConfig =
      addDescrKeys'
        ((myModMask, xK_F1), showKeybindings)
        myKeysDescr
        def
          { terminal = myTerminal,
            focusedBorderColor = "#8BE9FD",
            modMask = mod4Mask,
            borderWidth = 2,
            handleEventHook = dynamicHook <+> refocusLastWhen isFloat <+> handleEventHook def,
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
    myMainLayout =
      renamed [Replace "Tall"] tiled
        ||| renamed [Replace "Wide"] (Mirror tiled)
    myLayoutHook = smartBorders . refocusLastLayoutHook . trackFloating . mkToggle (single FULL) $ myMainLayout

myTerminal :: String
myTerminal = "kitty"

myModMask :: KeyMask
myModMask = mod4Mask

ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0 :: String
[ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0] =
  map show ([1 .. 10] :: [Integer])

myWorkspaces :: [String]
myWorkspaces = [ws1, ws2, ws3, ws4, ws5, ws6, ws7, ws8, ws9, ws0]

myStartupHook :: X ()
myStartupHook = do
  setWMName "LG3D"
  spawn "feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png"
  whenX isWork $ spawnOnce "rambox"

myDynamicStatusBar :: ScreenId -> IO StatusBarConfig
myDynamicStatusBar (S i) = statusBarPipe command $ pure pp
  where
    command = "xmonadFifo.sh " ++ show i
    pp = filterOutWsPP [scratchpadWorkspaceTag] myLogHook

restartPolybar :: MonadIO m => m ()
restartPolybar = spawn "systemctl --user restart polybar"

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
          className =? "Google-chrome" <||> className =? "Firefox" -?> doShift ws1,
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
   in [ subtitle "launching and killing programs",
        ( (modm, xK_Return),
          addName "Launch Terminal" $ spawn $ XMonad.terminal conf
        ),
        ( (modm, xK_d),
          addName "Open DRun menu" $
            spawn "rofi -show combi -combi-modi window,drun -show-icons"
        ),
        ( (modm .|. shiftMask, xK_d),
          addName "Open Run menu" $ spawn "rofi -show run -sidebar-mode"
        ),
        ((modm .|. shiftMask, xK_q), addName "Close the focused window" kill),
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
        ((modm, xK_F6), addName "Player previous" $ spawn "playerctl previous"),
        ((modm, xK_F7), addName "Player next" $ spawn "playerctl next")
      ]
        ++ subtitle "sound controls" :
      [ ((modm, xK_minus), addName "Decrease volume" $ spawn "pamixer -d 5 && volnoti-show $(pamixer --get-volume)"),
        ((modm, 0x1be), addName "Increase volume" $ spawn "pamixer -i 5 && volnoti-show $(pamixer --get-volume)"),
        ((modm, xK_equal), addName "Increase volume" $ spawn "pamixer -i 5 && volnoti-show $(pamixer --get-volume)")
      ]
        ++ subtitle "switching workspaces" :
      [ ((m .|. modm, k), addName (n ++ i) $ windows $ f i)
        | (f, m, n) <-
            [ (W.greedyView, 0, "Switch to workspace "),
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
            [ (viewScreen, 0, "View screen "),
              (sendToScreen, shiftMask, "Move to screen ")
            ]
      ]
        ++ [ subtitle "extra",
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

myLogHook :: PP
myLogHook =
  def
    { ppCurrent = wrap "[" "]",
      ppVisible = wrap "|" "|" . clickableWS,
      ppUrgent = wrap "%{o#f00}" "%{-o}" . clickableWS,
      ppHidden = wrap "" "" . clickableWS,
      ppWsSep = " ",
      ppSep = " : ",
      ppTitle = shorten 100
    }
  where
    clickableWS ws =
      maybe
        ws
        (\i -> "%{A1:xdotool set_desktop " ++ show i ++ ":}" ++ ws ++ "%{A}")
        $ elemIndex ws myWorkspaces

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

isWork :: MonadIO m => m Bool
isWork = io $ (== "adomas-jatuzis-nixos") <$> getHostName
