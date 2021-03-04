{-# OPTIONS_GHC -Wall -Werror -fno-warn-missing-signatures #-}

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Exception (bracket)
import Control.Monad
  ( join,
    when,
  )
import qualified DBus as D
import qualified DBus.Client as D
import Data.List (elemIndex)
import Data.Maybe (maybeToList)
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
import XMonad.Hooks.DynamicLog
  ( PP (..),
    dynamicLogWithPP,
    ppCurrent,
    ppHidden,
    ppOutput,
    ppSep,
    ppTitle,
    ppUrgent,
    ppVisible,
    ppWsSep,
    shorten,
    wrap,
  )
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    fullscreenEventHook,
  )
import XMonad.Hooks.ManageDocks
  ( ToggleStruts (ToggleStruts),
    avoidStruts,
    docks,
    manageDocks,
  )
import XMonad.Hooks.ManageHelpers
  ( composeOne,
    doCenterFloat,
    transience,
    (-?>),
  )
import XMonad.Hooks.SetWMName (setWMName)
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
    namedScratchpadFilterOutWorkspacePP,
    namedScratchpadManageHook,
  )
import XMonad.Util.Run
  ( runProcessWithInput,
    spawnPipe,
  )
import XMonad.Util.SpawnOnce (spawnOnce)

main :: IO ()
main = do
  dbus <- D.connectSession
  -- Request access to the DBus name
  _ <-
    D.requestName
      dbus
      (D.busName_ "org.xmonad.Log")
      [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad . ewmh . docks $
    myConfig
      { logHook =
          dynamicLogWithPP
            . namedScratchpadFilterOutWorkspacePP
            . myLogHook
            $ dbus
      }
  where
    myConfig =
      addDescrKeys'
        ((myModMask, xK_F1), showKeybindings)
        myKeysDescr
        def
          { terminal = myTerminal,
            focusedBorderColor = "#8BE9FD",
            modMask = mod4Mask,
            borderWidth = 2,
            handleEventHook = fullscreenEventHook <+> handleEventHook def,
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
    myLayoutHook = smartBorders . avoidStruts . mkToggle (single FULL) $ myMainLayout

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
  spawnOnce "feh --bg-max --image-bg white --no-fehbg ~/wallpaper.png"
  spawn "systemctl --user restart polybar.service"
  addEWMHFullscreen
  whenX isWork $ spawnOnce "rambox"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ spawnHook,
      className =? "lxqt-openssh-askpass" --> doCenterFloat,
      manageDocks,
      namedScratchpadManageHook myScratchpads,
      manageHook def
    ]
  where
    spawnHook =
      composeOne
        [ transience,
          className =? "Google-chrome" <||> className =? "Firefox" -?> doShift ws1,
          className =? "jetbrains-idea" -?> doShift ws3,
          className =? "Rambox" -?> doShift ws4,
          className =? "Steam" <||> className =? "SmartGit" -?> doShift ws5,
          className =? "libreoffice" -?> doShift ws6,
          className =? "Eclipse" -?> doShift ws7,
          className =? "KeePass2" -?> doShift ws9,
          className =? "Google Play Music Desktop Player" -?> doShift ws0,
          className =? "Spotify" -?> doShift ws0
        ]

myKeysDescr :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeysDescr conf@XConfig {XMonad.modMask = modm} =
  let ltKeys =
        [ 0x1b1, -- ą
          0x1e8, -- č
          0x1ea, -- ę
          0x3ec, -- ė
          0x3e7, -- į
          0x1b9, -- š
          0x3f9, -- ų
          0x3fe, -- ū
          0xafe, -- „
          0xad2 -- “
        ]
   in [ subtitle "launching and killing programs",
        ( (modm, xK_Return),
          addName "Launch Terminal" $ spawn $ XMonad.terminal conf
        ),
        ( (modm, xK_d),
          addName "Open DRun menu" $
            spawn "rofi -show combi -combi-modi window,drun"
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
        ((modm, xK_F4), addName "Power menu" $ spawn "rofi-powermenu"),
        ( (modm, xK_q),
          addName "Restart" $ spawn "xmonad --recompile && xmonad --restart"
        ),
        subtitle "scratchpads",
        ( (modm .|. controlMask, xK_e),
          addName "Emacs scratchpad" $ namedScratchpadAction myScratchpads nsEmacs
        ),
        ( (modm .|. controlMask, xK_s),
          addName "Terminal scratchpad" $
            namedScratchpadAction myScratchpads nsTerminal
        )
      ]
        ++ subtitle "music player controls" :
      [ ( (modm, xK_F5),
          addName "Player play-pause" $ spawn "playerctl play-pause"
        ),
        ((modm, xK_F6), addName "Player previous" $ spawn "playerctl previous"),
        ((modm, xK_F7), addName "Player next" $ spawn "playerctl next")
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

myLogHook :: D.Client -> PP
myLogHook dbus =
  def
    { ppOutput = dbusOutput dbus,
      ppCurrent = wrap "[" "]" . clickableWS,
      ppVisible = wrap "" "" . clickableWS,
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

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal =
        (D.signal objectPath interfaceName memberName)
          { D.signalBody = [D.toVariant $ UTF8.decodeString str]
          }
  D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

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

addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a <- getAtom "ATOM"
  liftIO $ do
    sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32
        dpy
        r
        a_NET_SUPPORTED
        a
        propModeAppend
        [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

isWork :: MonadIO m => m Bool
isWork = (== "work\n") <$> runProcessWithInput "autorandr" ["--detect"] ""
