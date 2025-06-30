{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Monad (liftM2, unless, when)
import Data.Monoid (All)
import Keybindings
import Layout
import Plugins.Soundtrack (getArtist)
import Preferences
import System.Exit (exitSuccess)
import Theme.Theme
import Workspaces
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.MostRecentlyUsed
import XMonad.Actions.NoBorders
import XMonad.Actions.Promote
import XMonad.Actions.Submap
import XMonad.Actions.TiledWindowDragging
import XMonad.Actions.WithAll
import XMonad.Hooks.DynamicIcons
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Cursor
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.WindowProperties

------------------------------------------------------------------------
-- Event handling
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook :: Event -> X All
myHandleEventHook =
  handleEventHook def
    <+> swallowEventHook
          (className
             =? "Alacritty"
             <||> className
             =? "St"
             <||> className
             =? "org.wezfurlong.wezterm"
             <||> className
             =? "kitty")
          ((not
              <$> (className
                     =? "St"
                     <||> className
                     =? "St-float"
                     <||> className
                     =? "org.wezfurlong.wezterm"
                     <||> className
                     =? "Dragon"
                     <||> className
                     =? "qemu-system-x86_64"
                     <||> className
                     =? "noswallow"))
             <||> className
             =? "re")

-- Enable transparency for inactive windows.
fadeHook :: X ()
fadeHook
  | Preferences.applyOnlyOnCurrentWS = fadeInactiveCurrentWSLogHook fadeAmount
  | otherwise = fadeInactiveLogHook fadeAmount

-- Main logHook function
myLogHook :: X ()
myLogHook =
  refocusLastLogHook >> nsHideOnFocusLoss onlyCertainScratchpads >> fadeHook
  where
    -- Anki contains more windows which may be used, for e.g., while creating a
    -- new deck of cards one may open up a new window which will otherwise hide
    -- the previous Anki window because focus was lost. This filter will ensure
    -- that all windows remain visible until specifically closed.
    onlyCertainScratchpads =
      filter (\(NS _n _ _ _) -> _n /= "Anki") myScratchpads

-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
--
mySB :: StatusBarConfig
mySB =
  statusBarProp
    "xmobar"
    (clickablePP
       =<< dynamicIconsPP
             myIconConfig
             (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPPForSB))
  where
    myXmobarPPForSB :: PP
    myXmobarPPForSB =
      def
        { ppSep = magenta " "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent =
            blue . wrap "" "" . xmobarBorder "Bottom" base06 2 . xmobarFont 2
        , ppHidden = lowWhite . wrap "" "" . xmobarFont 2
        , ppWsSep = xmobarColor "" background "  "
        , ppTitle =
            magenta . xmobarAction "xdotool key Super+shift+c" "2" . shrink 40
    -- , ppOrder         = \[ws, l, t, ex] -> [ws, l, ex, t]
    -- , ppExtras        = [xmobarColorL base01 background windowCount]
        , ppLayout =
            red
              . xmobarAction "xdotool key Super+space" "1"
              . xmobarAction "xdotool key Super+shift+space" "3"
              . (\case
                   "Tall" -> "<icon=Tall.xpm/>"
                   "Horizon" -> "<icon=Horizon.xpm/>"
                   "ThreeCol" -> "<icon=ThreeCol.xpm/>"
                   "Monocle" -> "<icon=Monocle.xpm/>"
                   _ -> "?")
        }
      where
        shrink :: Int -> String -> String
        shrink = shorten' "…"
        wrapSep :: String -> String
        wrapSep =
          wrap
            (xmobarColor base00 "" (xmobarFont 5 "\xe0b4"))
            (xmobarColor base00 "" (xmobarFont 5 "\xe0b6"))
        background :: String
        background = base00 ++ ":5"
        blue, lowWhite, magenta, red :: String -> String
        magenta = xmobarColor base05 background
        blue = xmobarColor base04 background
    -- purple   = xmobarColor "#bd93f9" "#2c323a:5"
    -- lowBlue  = xmobarColor "#8be9fd" "#2c323a:5"
    -- white    = xmobarColor "#f8f8f2" "#2c323a:5"
    -- yellow   = xmobarColor "#f1fa8c" "#2c323a:5"
        red = xmobarColor base01 background
        lowWhite = xmobarColor base07 background
    -- gray     = xmobarColor "" background
    -- green    = xmobarColor base02 background
    -- -- Get count of available windows on a workspace
    -- windowCount :: X (Maybe String)
    -- windowCount =
    --   gets
    --     $ Just
    --     . show
    --     . length
    --     . W.integrate'
    --     . W.stack
    --     . W.workspace
    --     . W.current
    --     . windowset
    myIconConfig :: IconConfig
    myIconConfig =
      def
        { iconConfigIcons = myIcons
        , iconConfigFmt = iconsFmtReplace (wrapUnwords "" "")
        , iconConfigFilter = iconsGetFocus
        }
      where
        myIcons :: Query [String]
        myIcons =
          composeAll
            [ className =? "discord" --> appIcon "<fn=3>\xf392</fn>"
            , className =? "Discord" --> appIcon "<fn=3>\xf268</fn>"
            , className =? "firefox" --> appIcon "<fn=3>\xf269</fn>"
            , className =? "Brave-browser" --> appIcon "<fn=3>\xf268</fn>"
            , className =? "St" --> appIcon "<fn=2>\xe795</fn>"
            , className =? "Emacs" --> appIcon "<fn=4>\xe926</fn>"
            , className =? "code-oss" --> appIcon "<fn=4>\xe60c</fn>"
            , className =? "Org.gnome.Nautilus" --> appIcon "<fn=1>\xf07b</fn>"
            , className =? "Spotify" --> appIcon "<fn=3>\xf1bc</fn>"
            , className =? "mpv" --> appIcon "<fn=1>\xf03d</fn>"
            , className =? "VirtualBox Manager" --> appIcon "<fn=4>\xea3e</fn>"
            , className =? "Lutris" --> appIcon "<fn=1>\xf11b</fn>"
            , className =? "Sxiv" --> appIcon "<fn=1>\xf03e</fn>"
            ]

myStartupHook :: X ()
myStartupHook = startupApps

myConfig =
  def
    { terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , clickJustFocuses = myClickJustFocuses
    , borderWidth = myBorderWidth
    , modMask = myModKey
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    -- Keybindings
    , keys = myKeys
    , mouseBindings = myMouseBindings
    -- Layouts
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook = myHandleEventHook
    , startupHook = myStartupHook
    , logHook = myLogHook
    }

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta "  "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, _] -> [ws, l]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    , ppLayout =
        \case
          "Columns" -> "<icon=Columns.xpm/>"
          "MagnifiedColumns" -> "<icon=MagnifiedColumns.xpm/>"
          "Full" -> "<icon=Full.xpm/>"
          "Tall" -> "<icon=Tall.xpm/>"
          "ThreeCol" -> "<icon=ThreeCol.xpm/>"
          "2-by-3 (left)" -> "<icon=TwoByThreeLeft.xpm/>"
          "2-by-3 (right)" -> "<icon=TwoByThreeRight.xpm/>"
          "Tiled" -> "<icon=Tiled.xpm/>"
          _ -> "<icon=XMonad.xpm/>"
    }
  where
    formatFocused = wrap (white "") (white "") . magenta . ppWindow
    formatUnfocused = wrap (grey "") (grey "") . grey . ppWindow
    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow =
      xmobarRaw
        . (\w ->
             if null w
               then "Untitled"
               else w)
        . shorten myXMonadLogAppLength
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    grey = xmobarColor "#808080" ""

main :: IO ()
main =
  xmonad
    -- . configureMRU -- Focus to most recently used window
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
