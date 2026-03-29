{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Monad (liftM2, unless, when)
import Data.Monoid (All)
import Keybindings
import Layout
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
      filter (\(NS _n _ _ _) -> _n /= "CalibreWeb")
        $ filter (\(NS _n _ _ _) -> _n /= "Anki") myScratchpads

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
    , ppCurrent = wrap " " "" . showCurrentWorkspace
    , ppHidden = white . wrap " " "" . showNamedWorkspaces
    , ppHiddenNoWindows = grey . wrap " " "" . showNamedWorkspaces
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, _] -> [ws, l]
    , ppExtras = [logTitles formatFocused formatUnfocused]
    , ppLayout =
        \case
          "Columns" -> "<icon=Columns.xpm/>"
          "MagnifiedColumns" -> "<icon=MagnifiedColumns.xpm/>"
          "Full" -> "<icon=Full.xpm/>"
          "Tall" -> "<icon=Tall.xpm/>"
          "ThreeCol" -> "<icon=MagnifiedColumns.xpm/>"
          "2-by-3 (left)" -> "<icon=TwoByThreeLeft.xpm/>"
          "2-by-3 (right)" -> "<icon=TwoByThreeRight.xpm/>"
          "2x3 LT" -> "<icon=TwoByThreeLeftWithTabs.xpm/>"
          "2x3 RT" -> "<icon=TwoByThreeRightWithTabs.xpm/>"
          "Tiled" -> "<icon=Tiled.xpm/>"
          _ -> "<icon=Unknown.xpm/>"
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
    lightBlue = xmobarColor "#8be9fd" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    darkYellow = xmobarColor "#fabd2f" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    grey = xmobarColor "#808080" ""
    showNamedWorkspaces wsId =
      if any (`elem` wsId) (unwords myWorkspaces)
        then "●"
        else ""
    showCurrentWorkspace wsId =
      if any (`elem` wsId) (unwords myWorkspaces)
        then "<icon=currentWorkspace.xpm/>"
        else ""

main :: IO ()
main =
  xmonad
    -- . configureMRU -- Focus to most recently used window
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig
