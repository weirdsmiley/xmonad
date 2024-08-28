{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main
  ( main
  ) where

import Control.Monad (liftM2)
import Data.Monoid (All)
import Keybindings
import Layout
import Preferences
import System.Exit (exitSuccess)
import Theme.Theme
import Workspaces
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.GroupNavigation
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
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing
import XMonad.Util.ClickableWorkspaces
import XMonad.Util.Cursor
import XMonad.Util.DynamicScratchpads
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Ungrab
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
                     =? "kitty"
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

-- myLogHook = refocusLastLogHook >> nsHideOnFocusLoss myScratchpads
myLogHook :: X ()
myLogHook
  | Preferences.applyOnlyOnCurrentWS = fadeInactiveCurrentWSLogHook fadeAmount
  | otherwise = fadeInactiveLogHook fadeAmount

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
             (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))
  where
    myXmobarPP :: PP
    myXmobarPP =
      def
        { ppSep = magenta " "
        , ppTitleSanitize = xmobarStrip
        , ppCurrent =
            blue . wrap "" "" . xmobarBorder "Bottom" base06 2 . xmobarFont 2
        , ppHidden = lowWhite . wrap "" "" . xmobarFont 2
        , ppWsSep = xmobarColor "" background "  "
        , ppTitle =
            magenta . xmobarAction "xdotool key Super+shift+c" "2" . shorten 40
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
        shorten :: Int -> String -> String
        shorten = shorten' "…"
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
myStartupHook = do
  setDefaultCursor xC_left_ptr
  last $ map spawnOnce startupApps

myConfig =
  def
    { terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , clickJustFocuses = myClickJustFocuses
    , borderWidth = myBorderWidth
    , modMask = altMask
    , workspaces = myWorkspaces
    , normalBorderColor = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    -- , keys = myKeys
    , layoutHook = myLayout
    , manageHook = myManageHook
    , handleEventHook = myHandleEventHook
    , startupHook = myStartupHook
    , logHook = myLogHook
    }
    `additionalKeysP` [ ("M-t", spawn myTerminal)
                      , ( "M-d"
                        , unsafeSpawn
                            (myLauncher
                               ++ " -show combi -combi-modi window,drun -modi combi -show-icons"))
                      , ("M-s", unGrab *> spawn "scrot -s")
                      , ("M-f", unGrab *> spawn "scrot")
                      ]

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " ▪ "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent = wrap " " "" . xmobarBorder "Bottom" "#8be9fd" 2
    , ppHidden = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent = red . wrap (yellow "!") (yellow "!")
    , ppOrder = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras = [logTitles formatFocused formatUnfocused]
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
        . shorten 20
    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor "#ff79c6" ""
    blue = xmobarColor "#bd93f9" ""
    white = xmobarColor "#f8f8f2" ""
    yellow = xmobarColor "#f1fa8c" ""
    red = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""
    grey = xmobarColor "#808080" ""

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main =
  xmonad
    . ewmhFullscreen
    . ewmh
    . docks
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    $ myConfig

------------------------------------------------------------------------
-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help =
  unlines
    [ "The default modifier key is 'super'. Default keybindings:"
    , ""
    , "-- launching and killing programs"
    , "mod-Shift-Enter      Launch terminal"
    , "mod-p                Launch XPrompt (Xmonad Prompt)"
    , "mod-c                Launch greenclip with rofi"
  --, "Alt-p                Launch dmenu"
  --, "Alt-c                Launch greenclip with dmenu"
    , "mod-Shift-c          Close/kill the focused window"
    , "mod-Space            Rotate through the available layout algorithms"
    , "mod-Shift-Space      Reset the layouts on the current workSpace to default"
    , "mod-n                Resize/refresh viewed windows to the correct size"
    , ""
    , "-- move focus up or down the window stack"
  -- , "mod-Tab              Move focus to the next window"
  -- , "mod-Shift-Tab        Move focus to the previous window"
    , "mod-j                Move focus to the next window"
    , "mod-k                Move focus to the previous window"
    , "mod-m                Move focus to the master window"
    , ""
    , "-- modifying the window order"
    , "mod-Return           Move the focused window to the master pane."
    , "mod-Shift-j          Swap the focused window with the next window"
    , "mod-Shift-k          Swap the focused window with the previous window"
    , ""
    , "-- resizing the master/slave ratio"
    , "mod-h                Shrink the master width"
    , "mod-l                Expand the master width"
    , "mod-alt-j            Shrink the master height"
    , "mod-alt-k            Expand the master height"
    , ""
    , "-- increase or decrease spacing (gaps)"
    , "mod-g                Toggle spacing/gaps"
    , "mod-i                Increment both screen and window borders"
    , "mod-d                Deincrement both screen and window borders"
    , "Alt-i                Increment screen borders"
    , "Alt-d                Deincrement screen borders"
    , "Alt-Shift-i          Increment window borders"
    , "Alt-Shift-d          Deincrement window borders"
    , ""
    , "-- floating layer support"
    , "mod-t                Push window back into tiling; unfloat and re-tile it"
    , "mod-shift-t          Push all floating windows on screen into tilling"
    , ""
    , "-- increase or decrease number of windows in the master area"
    , "mod-comma  (mod-,)   Increment the number of windows in the master area"
    , "mod-period (mod-.)   Deincrement the number of windows in the master area"
    , ""
    , "-- quit, or restart"
    , "mod-Shift-q          Quit xmonad"
    , "mod-q                Restart xmonad"
    , ""
    , "-- Workspaces & screens"
    , "mod-[1..9]           Switch to workSpace N"
    , "mod-Shift-[1..9]     Move client to workspace N"
    , "mod-Control-[1..9]   Move client and switch to workspace N"
    , "mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3"
    , "mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3"
    , "mod-Right            Switch to next workSpace"
    , "mod-Left             Switch to previous workSpace"
    , "mod-Shift-Right      Move client to next workSpace"
    , "mod-Shift-Left       Move client to previous workSpace"
    , "mod-z                Switch between previously used workSpace"
    , "mod-f                Focused window goes fullscreen"
    , ""
    , "-- Cycle Workspaces"
    , "mod-Tab              Cycle between active workspaces from left to right"
    , "mod-Shift-Tab        Cycle between active workspaces from right to left"
    , "mod-left             Go to next workspace"
    , "mod-right            Go to preview workspace"
    , "mod-Shift-left       Move focused window to next workspace"
    , "mod-Shift-right      Move focused window to previous workspace"
    , ""
    , "-- Mouse bindings: default actions bound to mouse events"
    , "mod-button1          Set the window to floating mode and move by dragging"
    , "mod-button2          Raise the window to the top of the stack"
    , "mod-button3          Set the window to floating mode and resize by dragging"
    , ""
    , "-- Switch layouts"
    , "Alt-t                Switch to 'Tall' layout"
    , "Alt-c                Switch to 'ThreeColMid' layout"
    , "Alt-f                Switch to 'Full' layout"
    , ""
    , "-- Sublayout bindings"
    , "mod-Ctrl-h           Merge with left client"
    , "mod-Ctrl-l           Merge with right client"
    , "mod-Ctrl-k           Merge with upper client"
    , "mod-Ctrl-j           Merge with lower client"
    , "mod-Ctrl-Space       Switch to next sublayout"
    , "mod-Ctrl-m           Merge all available clients on the workspace"
    , "mod-Ctrl-u           Unmerge currently focused client"
    , "mod-Ctrl-period (.)  Move focus to the next window in the sublayout"
    , "mod-Ctrl-comma (,)   Move focus to the previous window in the sublayout"
    , ""
    , "-- Copy windows"
    , "mod-v                Copy focused window to all workspaces"
    , "mod-Shift-v          Only keep currently focused copied window"
    , ""
    , "-- Scratchpad"
    , "mod-Ctrl-Enter       Open a terminal in scratchpad"
    , "mod-Shift-Equal (=)  Add currently focused window to scratchpad (1)"
    , "mod-Equal (=)        Open scratchpad (1)"
    , "mod-Shift-minus (-)  Add currently focused window to scratchpad (2)"
    , "mod-minus (-)        Open scratchpad (2)"
    , ""
    , "-- Shortcuts for taking screenshots"
    , "Print                Take fullscreen screenshot"
    , "Shift-Print          Take screenshot of selected screen"
    , "Ctrl-Print           Take screenshot of focused window"
    , ""
    , "-- Application"
  -- , "All-e                Open emacs-client"
    , "alt-b                Open browser (Firefox)"
    , "alt-F9               Turn on/off picom"
    ]
--- vim:ft=haskell:expandtab
