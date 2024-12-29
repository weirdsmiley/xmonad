module Preferences
  ( myTerminal
  , myNamedTerminal
  , myCodeSprintTerm
  , myWritingTerm
  , myResearchTerm
  , myBrowser
  , myRegularFont
  , myMonospaceFont
  , myPdfViewer
  , myWallpaper
  , myKanboardProfile
  , myKanboardUrl
  , myLauncher
  , myScratchpads
  , myScreenShotter
  , myGaps
  , myManageHook
  , myXMonadLogAppLength
  , myFocusedBorderColor
  , myNormalBorderColor
  , altMask
  , myBorderWidth
  , myClickJustFocuses
  , myFocusFollowsMouse
  , fadeAmount
  , startupApps
  , applyOnlyOnCurrentWS
  ) where

import Workspaces
import XMonad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

-- Function to name terminals
-- > Named terminals to rule them all,
-- > Named terminals to find them
myNamedTerminal name = myTerminal ++ " --title " ++ name

myTerminal, myCodeSprintTerm, myWritingTerm, myResearchTerm, myBrowser, myRegularFont, myMonospaceFont, myPdfViewer, myWallpaper ::
     String
-- Default terminal
myTerminal = "kitty"

-- My custom terminal pads
myCodeSprintTerm = myNamedTerminal "Codesprint"

myWritingTerm = myNamedTerminal "Writing"

myResearchTerm = myNamedTerminal "Research"

myBrowser = "firefox"

myKanboardProfile = "Kanboard"

-- This url is opened in myBrowser (kiosk-mode, and myKanboardProfile) with a
-- keybinding.
myKanboardUrl =
  "http://localhost:8081/?controller=Bigboard&action=index&plugin=Bigboard"

myPdfViewer = "/home/manas/.local/bin/sioyek"

myScreenShotter = "scrot"

-- Fonts
myRegularFont = "Noto Sans 10"

myMonospaceFont = "Fira Code Regular 9"

-- Rofi launcher
myLauncher :: String
myLauncher =
  "rofi -width 424 -lines 8"
    ++ " -font '"
    ++ myRegularFont
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
myModMask, altMask :: KeyMask
myModMask = mod4Mask

altMask = mod1Mask

myWallpaper = "~/.wallpaper/matte-pack/gazing-beyond-neon-peaks-jo.jpg"

nightLight = "redshift -O 2600"

xmobar = "/home/manas/.local/bin/xmobar"

-- Applications to boot at startup. They are only spawned once.
startupApps :: [String]
startupApps =
  [ "feh --no-fehbg --bg-scale " ++ myWallpaper
  , "picom --config /home/manas/.config/picom/picom.conf"
  , "xrdb -merge /home/manas/.Xresources"
  , nightLight
  , myCodeSprintTerm
  , myWritingTerm
  , myResearchTerm
  , myBrowser
  , myPdfViewer
  , Preferences.xmobar
  , "Discord"
  , "Zulip"
  , "gimp"
  , "zoom"
  , "telegram-desktop"
  , "gnome-boxes"
  ]

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
    [ className =? "MPlayer" -?> doFloat
    , resource =? "desktop_window" -?> doIgnore
    , resource =? "kdesktop" -?> doIgnore
    , resource =? "Toolkit" <||> resource =? "Browser" -?> doFloat
    , resource =? "redshift-gtk" -?> doCenterFloat
    , className =? "Gammastep-indicator" -?> doCenterFloat
    , className =? "ibus-ui-gtk3" -?> doIgnore
    , resource =? "gcr-prompter" -?> doCenterFloat
    , className =? "St-float" -?> doFloat
    , transience
    , title =? "XMonad Keybind" -?> doCenterFloat
    , className =? "Ibus-extension-gtk3" -?> doFloat
    , isFullscreen -?> doFullFloat
    , isDialog -?> doCenterFloat
    , title =? "Codesprint" -?> doShift (head myWorkspaces)
    , title =? "Writing" -?> doShift (myWorkspaces !! 1)
    , className =? "sioyek" -?> doShift (myWorkspaces !! 1)
    , className =? "Zathura" -?> doShift (myWorkspaces !! 1)
    , title =? "Research" -?> doShift (myWorkspaces !! 2)
    , className =? "zoom" -?> doShift (myWorkspaces !! 4)
    , className =? "discord" -?> doShift (myWorkspaces !! 5)
    , className =? "Zulip" -?> doShift (myWorkspaces !! 5)
    , title =? "Telegram" -?> doShift (myWorkspaces !! 6)
    , className =? "steam" -?> doShift (myWorkspaces !! 7)
    , className =? "gimp" -?> doShift (myWorkspaces !! 7)
    , className =? "qemu" -?> doShift (myWorkspaces !! 8)
    , title =? "Boxes" -?> doShift (myWorkspaces !! 8)
    , title =? "Pomodoro" -?> doCenterFloat
    , title =? "Camera" -?> doCenterFloat
    , className =? "Kanboard" -?> doFullFloat
    , className =? "gnome-calculator" -?> doCenterFloat
    , className
        =? "VirtualBox Manager"
        <||> className
        =? "gnome-boxes"
        -?> doShift (myWorkspaces !! 8)
    ]
    <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads
                 -- run a terminal inside scratchpad
 = [NS "terminal" spawnTerm findTerm manageTerm]
  where
    spawnTerm =
      myTerminal
        ++ " --class scratchpad --session ~/.config/kitty/sessions/xmonad-scratchpad"
    findTerm = className =? "scratchpad"
    manageTerm = customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (3 / 4)

-- Border colors for unfocused and focused windows
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = "black"

myFocusedBorderColor = "grey"

-- Control how long XMonadLog should show application name in xmobar.
myXMonadLogAppLength :: Int
myXMonadLogAppLength = 20
