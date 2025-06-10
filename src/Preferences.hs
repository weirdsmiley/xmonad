module Preferences
  ( myTerminal
  , myNamedTerminal
  , myCodeSprintTerm
  , myWritingTerm
  , myResearchTerm
  , myBrowser
  , myPdfViewer
  , myWallpaper
  , myKanboardProfile
  , myKanboardUrl
  , myLauncher
  , myScratchpads
  , myScreenShotter
  , myGaps
  , myNotifHandler
  , myManageHook
  , myXMonadLogAppLength
  , myFocusedBorderColor
  , myNormalBorderColor
  , myModKey
  , myBorderWidth
  , myClickJustFocuses
  , myFocusFollowsMouse
  , fadeAmount
  , startupApps
  , applyOnlyOnCurrentWS
  , myMusicCtrl
  , myPomodoro
  , myCustomCommands
  , isTerminal
  ) where

import Theme.Font
import Workspaces
import XMonad
import XMonad.Actions.Commands
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import XMonad.Util.Cursor
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (unsafeSpawn)
import XMonad.Util.SpawnOnce

isTerminal :: Query Bool
isTerminal = fmap (`elem` names) className
  where
    names :: [String]
    names =
      [myTerminal, myCodeSprintTerm, myWritingTerm, myResearchTerm]
        ++ [scratchpadWorkspaceTag]

-- Function to name terminals
-- > Named terminals to rule them all,
-- > Named terminals to find them
myNamedTerminal xs = myTerminal ++ " --title " ++ xs

-- Any configuration related to title or class should be inside the session file
-- itself.
mySessionedTerminal sess = myTerminal ++ " --session " ++ sess

myCustomTerminals title session =
  myTerminal ++ " --title '" ++ title ++ "' --session '" ++ session ++ "'"

myTerminal, myCodeSprintTerm, myWritingTerm, myResearchTerm, myBrowser, myPdfViewer, myWallpaper ::
     String
-- Default terminal
myTerminal = "kitty"

-- My custom terminal pads
myCodeSprintTerm =
  myCustomTerminals "CodeSprint" "~/.config/kitty/sessions/codesprint"

myWritingTerm = myCustomTerminals "Writings" "~/.config/kitty/sessions/writings"

myResearchTerm =
  myCustomTerminals "Research" "~/.config/kitty/sessions/research"

myBrowser = "firefox"

myKanboardProfile = "Kanboard"

-- This url is opened in myBrowser (kiosk-mode, and myKanboardProfile) with a
-- keybinding.
myKanboardUrl
  -- "http://localhost:8081/?controller=Bigboard&action=index&plugin=Bigboard"
 = "http://192.168.0.108/?controller=Bigboard&action=index&plugin=Bigboard"

myPdfViewer = "sioyek"

myScreenShotter = "scrot"

-- Rofi launcher
myLauncher :: String
myLauncher =
  "rofi -width 424 -lines 8"
    ++ " -font '"
    ++ myFont
    ++ "'"
    ++ " -show combi -combi-modi window,drun -modi combi -show-icons"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse, myClickJustFocuses :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 0

myGaps :: Num p => p
myGaps = 5

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
altKey = mod1Mask

metaKey = mod4Mask

myModKey :: KeyMask
myModKey = altKey

myWallpaper = "~/.config/xmonad/preview/wallpaper.jpg"

nightLight = "redshift -O 2600"

xmobar = "~/.local/bin/xmobar"

myNotifHandler = "dunstctl"

-- Applications to boot at startup. They are only spawned once.
startupApps :: X ()
startupApps = do
  setDefaultCursor xC_left_ptr
  spawnOnce $ "feh --no-fehbg --bg-scale " ++ myWallpaper
  spawnOnce "picom --config /home/manas/.config/picom/picom.conf"
  spawnOnce "xrdb -merge /home/manas/.Xresources"
  spawnOnce "setxkbmap -option ctrl:nocaps"
  spawnOnce "xcape -e 'Control_L=Escape' -t 1000"
  spawnOnce nightLight
  spawnOnce Preferences.xmobar
  spawnOnce myCodeSprintTerm
  spawnOnce myWritingTerm
  spawnOnce myResearchTerm
  spawnOnce myBrowser
  spawnOnce myPdfViewer
  spawnOnce "Discord"
  spawnOnce "Zulip"
  -- spawnOnce "gimp"
  spawnOnce "zoom"
  spawnOnce "telegram-desktop"
  spawnOnce "signal-desktop"
  spawnOnce "gnome-boxes"
  spawnOnce "zotero"

-- Apply any configurations only on current workspace and not on all workspaces.
-- If you have a multi-monitor setup, you may want to specifically make
-- configurations on your current monitor instead of every monitor following the
-- same configuration.
-- For e.g., if you want to make inactive windows translucent, then you can
-- decide if you want this configuration on all monitors or only your currently
-- focused monitor.
applyOnlyOnCurrentWS :: Bool
applyOnlyOnCurrentWS = True

-- Amount by which to fade inactive windows on current workspace only.
fadeAmount :: Rational
fadeAmount = 1.0

-- Music controller tool
myMusicCtrl = "playerctl"

------------------------------------------------------------------------
-- Window rules:
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- workspace number starts from 0
--
myManageHook :: ManageHook
myManageHook =
  composeOne
    [ title =? "CodeSprint" -?> doShift (head myWorkspaces)
    , title =? "Writings" -?> doShift (myWorkspaces !! 1)
    , className =? "sioyek" -?> doShift (myWorkspaces !! 1)
    , className =? "Zathura" -?> doShift (myWorkspaces !! 1)
    , title =? "Research" -?> doShift (myWorkspaces !! 2)
    , className =? "Zotero" -?> doShift (myWorkspaces !! 2)
    , className =? "zoom" -?> doShift (myWorkspaces !! 4)
    , className =? "discord" -?> doShift (myWorkspaces !! 5)
    , className =? "Zulip" -?> doShift (myWorkspaces !! 5)
    , className =? "TelegramDesktop" -?> doShift (myWorkspaces !! 6)
    , className =? "Signal" -?> doShift (myWorkspaces !! 6)
    , className =? "steam" -?> doShift (myWorkspaces !! 7)
    , className =? "Gimp" -?> doShift (myWorkspaces !! 7)
    , className =? "qemu" -?> doShift (myWorkspaces !! 8)
    , className =? "Org.gnome.Boxes" -?> doShift (myWorkspaces !! 8)
    , className =? "Gnome-pomodoro" -?> doFullFloat
    , className =? "Rhythmbox" -?> doCenterFloat
    , appName =? "blueman-manager" -?> doCenterFloat
    , appName =? "snapshot" -?> doCenterFloat
    , className =? "Gammastep-indicator" -?> doCenterFloat
    , className =? "Ibus-extension-gtk3" -?> doFloat
    , className =? "Kanboard" -?> doFullFloat
    , className =? "MPlayer" -?> doFloat
    , className =? "St-float" -?> doFloat
    , className =? "gnome-calculator" -?> doCenterFloat
    , className =? "ibus-ui-gtk3" -?> doIgnore
    , className =? "zenity" -?> doCenterFloat
    , className =? "org.gnome.clocks" -?> doCenterFloat
    , className =? "org.gnome.Weather" -?> doCenterFloat
    , className =? "feh" -?> doCenterFloat
    , className =? "loupe" -?> doCenterFloat
    , title =? "GNOME Tweaks" -?> doCenterFloat
    , className =? "pavucontrol" -?> doCenterFloat
    , isDialog -?> doCenterFloat
    , isFullscreen -?> doFullFloat
    , resource =? "Toolkit" <||> resource =? "Browser" -?> doFloat
    , resource =? "desktop_window" -?> doIgnore
    , resource =? "gcr-prompter" -?> doCenterFloat
    , resource =? "kdesktop" -?> doIgnore
    , resource =? "redshift-gtk" -?> doCenterFloat
    , title =? "XMonad Keybind" -?> doCenterFloat
    , transience
    ]
    <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads
                 -- run a terminal inside scratchpad
 =
  [ NS "terminal" spawnTerm findTerm manageTerm
  , NS "Kanboard" spawnKanboard (className =? "Kanboard") doFullFloat
  ]
  where
    spawnTerm =
      myTerminal
        ++ " --class scratchpad --session ~/.config/kitty/sessions/xmonad-scratchpad"
    spawnKanboard =
      myBrowser
        ++ " --class "
        ++ myKanboardProfile
        ++ " --new-window --kiosk -P "
        ++ myKanboardProfile
        ++ " '"
        ++ myKanboardUrl
        ++ "'"
    findTerm = className =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (3 / 4)

-- Border colors for unfocused and focused windows
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "black"

myFocusedBorderColor = "grey"

-- Control how long XMonadLog should show application name in xmobar.
myXMonadLogAppLength :: Int
myXMonadLogAppLength = 16

-- Pomodoro app
myPomodoro = "gnome-pomodoro"

myCustomCommands :: X [(String, X ())]
myCustomCommands = do
  return
    [ ( "swap-escape"
      , unsafeSpawn "/home/manas/workspace/scripts/swap-escape.sh")
    , ("kill-lsps", unsafeSpawn "/home/manas/workspace/scripts/kill-lsps.sh")
    , ("xkcd", unsafeSpawn "/home/manas/workspace/scripts/xkcd")
    , ("savereason", unsafeSpawn "/home/manas/workspace/scripts/save-reason.sh")
    , ("screenkey", unsafeSpawn "screenkey")
    , ("screenkey-off", unsafeSpawn "pkill -9 screenkey")
    ]
