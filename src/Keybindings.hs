module Keybindings
  ( myKeys
  , myMouseBindings
  , myXPConfig
  ) where

import Control.Monad (liftM2, void)
import Data.List (intercalate)
import qualified Data.Map as M
import Graphics.X11.ExtraTypes.XF86
import Plugins.Soundtrack (getRunningPlayer')
import Preferences
import System.Exit (exitSuccess)
import Theme.Font
import Theme.Theme
import Workspaces
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.GroupNavigation
import XMonad.Actions.NoBorders (toggleBorder)
import XMonad.Actions.PerWindowKeys
import XMonad.Actions.Promote (promote)
import XMonad.Actions.Submap
import XMonad.Actions.TiledWindowDragging (dragWindow)
import XMonad.Actions.WithAll
import XMonad.Layout.Hidden
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import XMonad.Prompt.FuzzyMatch
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad
  ( namedScratchpadAction
  , scratchpadWorkspaceTag
  )
import XMonad.Util.Paste (sendKey)
import XMonad.Util.Run

--------------------------------------------------------------------------------
-- This takes in a major key and a list of subkeys with their descriptions (what
-- they do), and actions (how to do it). It returns a combined list of normal
-- mode submap keys and visual mode keys.
--
-- Example - If we have to create a submap with modm+a followed by m/n/p as
-- subkeys, then we put the subkeys in a list with their descriptions (:=
-- String) and what they do (:= X () (generally)).
--
--   makeChords (modm, xK_a)
--     [ ((0, xK_m), "description 1", spawn task1)
--     , ((0, xK_n), "description 2", spawn task2)
--     , ((0, xK_p), "description 3", spawn task3)
--     ]
--
-- returns two lists concatenated together. First is the set of normal mode
-- keys, and second is the set of visual mode keys.
--   [ ( (modm, xK_a)
--     , submap . M.fromList
--         $ [ ((0, xK_m), spawn task1)
--           , ((0, xK_n), spawn task2)
--           , ((0, xK_p), spawn task3)
--           ])
--   ]
--   ++
--   [ ( (modm, xK_a)
--     , visualSubmap def
--         $ M.fromList
--         $ map
--             (\(key, desc, action) -> ((0, key), (desc, action)))
--             [ (xK_m, "description 1", spawn task1)
--             , (xK_n, "description 2", spawn task2)
--             , (xK_p, "description 3", spawn task3)
--             ])
--   ]
--
makeChords :: a -> [((KeyMask, KeySym), String, X ())] -> [(a, X ())]
makeChords majorKey subKeys =
  (majorKey, submap . M.fromList $ map (\(k, _, a) -> (k, a)) subKeys)
    : [ ( majorKey
        , visualSubmap def
            $ M.fromList
            $ map (\(k, d, a) -> (k, (d, a))) subKeys)
      ]

--------------------------------------------------------------------------------
screenshotChords :: [((KeyMask, KeySym), X ())]
screenshotChords =
  [ ( (0, xK_Print)
    , unGrab *> safeSpawn myScreenShotter ["-f", "--quality", "100"])
  , ( (0 .|. controlMask, xK_Print)
    , unGrab *> safeSpawn myScreenShotter ["-u", "--quality", "100"])
  , ( (0 .|. shiftMask, xK_Print)
    , unGrab *> safeSpawn myScreenShotter ["-s", "--quality", "100"])
  ]

--------------------------------------------------------------------------------
-- Chords for anything related to focusing windows.
focusChords modm =
  [ ((modm, xK_c), kill1) -- Close focused window
  , ((modm, xK_k), windows W.focusUp) -- Focus on previous window
  , ((modm, xK_j), windows W.focusDown) -- Focus on next window
  -- , ((modm, xK_m), windows W.focusMaster) -- Focus on master window
  , ((modm, xK_s), promote) -- Swap focused window with master
  -- TODO: Focus to most recent window
  -- , ((modm, xK_Tab), moveTo Next nonEmptyNSP)
  , ((modm, xK_Tab), windows W.focusDown)
  -- FIXME: Jump to windows in recently-used order
  , ((modm .|. controlMask, xK_Right), nextMatch Forward (className =? ""))
  , ((modm .|. controlMask, xK_Left), nextMatch Backward (className =? ""))
  ]

--------------------------------------------------------------------------------
-- Hide windows from workspace
hiddenChords modm =
  makeChords
    (modm, xK_h)
    [ ((0, xK_h), "hide focused window", withFocused hideWindow)
    , ((shiftMask, xK_h), "pop last hidden window", popOldestHiddenWindow)
    ]

--------------------------------------------------------------------------------
resizeChords modm =
  [ ((modm .|. shiftMask, xK_l), sendMessage Expand) -- Expand master area
  -- TODO: This will do the opposite in twoByThreeLayout kinds.
  , ((modm .|. shiftMask, xK_h), sendMessage Shrink) -- Shrink master area
  , ((modm, xK_t), withFocused $ windows . W.sink) -- Push floatings back to tiling
  , ((modm .|. shiftMask, xK_t), sinkAll) -- Push all floating windows back to tiling
  ]

--------------------------------------------------------------------------------
-- Rotate through the available layout algorithms
layoutChords modm = [((modm, xK_space), sendMessage NextLayout)]

--------------------------------------------------------------------------------
-- mod-[1..9], Switch to workspace N
-- mod-shift-[1..9], Move client to workspace N
workspaceChords conf@XConfig {XMonad.modMask = modm} =
  [ ((m .|. modm, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0]
  , (f, m) <-
      [ (W.greedyView, 0)
      , (W.shift, shiftMask)
      , (liftM2 (.) W.greedyView W.shift, controlMask)
      ]
  ]
    ++
     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
     --
     --
     [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
     ]
    ++
     -- Move back and forth between recently used workspace
     -- Using super modifier.
     [((mod4Mask, xK_Tab), toggleWS' [scratchpadWorkspaceTag])]
    ++ [ ((modm .|. shiftMask, xK_k), moveTo Prev nonNSP)
       , ((modm .|. shiftMask, xK_j), moveTo Next nonNSP)
       ]
  where
    nonNSP = ignoringWSs [scratchpadWorkspaceTag]

--------------------------------------------------------------------------------
-- Keychords for standalone applications.
applicationChords modm =
  [ ((modm, xK_d), unsafeSpawn myLauncher) -- Open rofi launcher
  , ((modm .|. shiftMask, xK_Return), spawn myTerminal) -- Open new terminal
  , ((modm, xK_b), spawn myBrowser) -- Open browser
  ]

--------------------------------------------------------------------------------
-- Sound related chords
soundChords modm =
  makeChords
    (modm, xK_a)
    [ ( (0, xK_m)
      , "play/pause music"
      , getRunningPlayer' >>= \player ->
          spawn $ myMusicCtrl ++ " -p \"" ++ player ++ "\" play-pause")
    , ( (0, xK_p)
      , "previous track"
      , getRunningPlayer' >>= \player ->
          spawn $ myMusicCtrl ++ " -p \"" ++ player ++ "\" previous")
    , ( (0, xK_n)
      , "next track"
      , getRunningPlayer' >>= \player ->
          spawn $ myMusicCtrl ++ " -p \"" ++ player ++ "\" next")
    , ( (0, xK_k)
      , "increase volume"
      , spawn
          "pactl set-sink-mute @DEFAULT_SINK@ 0 && pactl set-sink-volume @DEFAULT_SINK@ +1%")
    , ( (0, xK_j)
      , "decrease volume"
      , safeSpawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-1%"])
    , ( (0, xK_l)
      , "mute volume"
      , safeSpawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"])
  -- , ( (mod4Mask, xF86XK_AudioMute)
  --   , safeSpawn "pactl" ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"])
  -- , ( (mod4Mask, xF86XK_AudioRaiseVolume)
  --   , spawn
  --       "pactl set-source-mute @DEFAULT_SOURCE@ 0 && pactl set-source-volume @DEFAULT_SOURCE@ +1%")
  -- , ( (mod4Mask, xF86XK_AudioLowerVolume)
  --   , safeSpawn "pactl" ["set-source-volume", "@DEFAULT_SOURCE@", "-1%"])
    ]

--------------------------------------------------------------------------------
-- Pomodoro session chords
pomodoroChords modm =
  makeChords
    (modm, xK_p)
    [ ( (0, xK_p)
      , "pause/resume pomodoro session"
      , spawn $ myPomodoro ++ " --pause-resume")
    , ((0, xK_n), "start 60 min session", spawn $ myPomodoro ++ " --start")
    , ((0, xK_s), "skip this session/break", spawn $ myPomodoro ++ " --skip")
    -- -- TODO: --extend requires a digit as arg. This requires additional
    -- -- effort.
    -- , ((0, xK_e), "extend this session", spawn $ myPomodoro ++ " --extend")
    ]

--------------------------------------------------------------------------------
-- FIXME: Chords for copying and pasting easily
copyPasteChords =
  [ ( (controlMask, xK_c)
    , bindFirst
        [ ( isTerminal
          , xdotool ["keyup", "c", "key", "--clearmodifiers", "XF86Copy"])
        , (pure True, sendKey controlMask xK_c)
        ])
  , ( (controlMask, xK_v)
    , bindFirst
        [ ( isTerminal
          , xdotool ["keyup", "v", "key", "--clearmodifiers", "XF86Paste"])
        , (pure True, sendKey controlMask xK_v)
        ])
  ]
  where
    xdotool :: [String] -> X ()
    xdotool args = void (runProcessWithInput "xdotool" args "")
    commands :: [(String, X ())]
    commands =
      [ ("shrink", sendMessage Shrink)
      , ("expand", sendMessage Expand)
      , ("refresh", refresh)
      ]

--------------------------------------------------------------------------------
-- Notification chords
notificationChords modm =
  makeChords
    (modm, xK_n)
    [ ( (0, xK_p)
      , "previous notification"
      , safeSpawn myNotifHandler ["history-pop"])
    , ( (0, xK_c)
      , "clear all notifications"
      , safeSpawn myNotifHandler ["close-all"])
    , ( (shiftMask, xK_c)
      , "delete all notifications from history"
      , safeSpawn myNotifHandler ["history-clear"])
    , ( (0, xK_t)
      , "toggle notifications"
      , safeSpawn myNotifHandler ["set-paused", "toggle"])
    ]

--------------------------------------------------------------------------------
-- Custom chords
customChords modm = [((modm, xK_y), myCustomCommands >>= runCommand)]

--------------------------------------------------------------------------------
-- All keybindings
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList
    $ [ ( (modm .|. shiftMask, xK_q)
        , confirmPrompt myXPConfig "Quit" $ io exitSuccess -- Quit XMonad
         )
      -- Restart XMonad
      , ( (modm, xK_q)
        -- , unsafeSpawn
        --     "xmonad --recompile; pkill -9 xmobar; xmonad --restart; ~/.local/bin/xmobar &")
        , safeSpawn "xmonad" ["--recompile"]
            >> safeSpawn "pkill" ["-9", "xmobar"]
            >> safeSpawn "xmonad" ["--restart"]
            >> unsafeSpawn "~/.local/bin/xmobar &")
      -- Show help page
      , ( (modm .|. shiftMask, xK_slash)
        , unsafeSpawn
            ("GTK_THEME=Adwaita:dark zenity --width 600 --height 800 --list --title='XMonad Keybindings' --text='Default modifier (mod) key is 'alt'.' --column='Keymaps' --column='Description' \""
               ++ help
               ++ "\""))
      -- Toggle fullscreen -- TODO: This won't work in set PerWorkspace hooks.
      , ((modm, xK_f), sendMessage $ Toggle NBFULL)
      -- Toggle gaps
      , ( (modm, xK_g)
        , sequence_ [toggleScreenSpacingEnabled, toggleWindowSpacingEnabled])
      -- Open Scratchpad
      , ((modm, xK_Return), namedScratchpadAction myScratchpads "terminal")
      -- Lock screen
      , ( (modm, xK_l)
        , unGrab
            *> safeSpawn
                 "env"
                 [ "XSECURELOCK_NO_COMPOSITE=1"
                 , "XSECURELOCK_AUTH_CURSOR_BLINK=0"
                 , "XSECURELOCK_BACKGROUND_COLOR=#2c0843"
                 , "XSECURELOCK_AUTH_BACKGROUND_COLOR=#611095"
                 , "XSECURELOCK_PASSWORD_PROMPT=hidden"
                 , "XSECURELOCK_SHOW_DATETIME=1"
                 , "xsecurelock"
                 ])
      -- Open Kanboard session
      , ( (modm, xK_x)
        , unGrab
            *> safeSpawn
                 myBrowser
                 [ "--class"
                 , myKanboardProfile
                 , "--new-window"
                 , "--kiosk"
                 , "-P"
                 , myKanboardProfile
                 , myKanboardUrl
                 ])
      -- Toggle a window floating in center
      -- TODO: This should be mapped to modm+f
      -- , ((modm, xK_p), withFocused toggleFloat)
      -- Play/pause via headphones
      , ( (noModMask, xF86XK_AudioPause)
        , getRunningPlayer' >>= \player ->
            spawn $ myMusicCtrl ++ " -p \"" ++ player ++ "\" pause")
      , ( (noModMask, xF86XK_AudioPlay)
        , getRunningPlayer' >>= \player ->
            spawn $ myMusicCtrl ++ " -p \"" ++ player ++ "\" play")
      ]
        ++ layoutChords modm
        ++ resizeChords modm
        ++ focusChords modm
        -- ++ hiddenChords modm
        ++ screenshotChords
        ++ applicationChords modm
        ++ [last $ soundChords modm]
        ++ workspaceChords conf
        ++ [last $ pomodoroChords modm]
        -- ++ copyPasteChords -- TODO
        ++ notificationChords modm
        ++ customChords modm
  where
    nonNSP = ignoringWSs [scratchpadWorkspaceTag]
    nonEmptyNSP =
      hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]

------------------------------------------------------------------------
-- XPrompt
--
myXPConfig :: XPConfig
myXPConfig =
  def
    { font = "xft:Hack:pixelsize=12"
    , bgColor = base04
    , fgColor = basebg
    , bgHLight = base04
    , fgHLight = base00
    , borderColor = base00
    , promptBorderWidth = 1
    , promptKeymap = defaultXPKeymap
    , position = Bottom
    -- , position = CenteredAt {xpCenterY = 0.3, xpWidth = 0.3}
    , alwaysHighlight = True -- Disables tab cycle
    , height = 30
    , maxComplRows = Just 10
    , historySize = 50
    , historyFilter = deleteAllDuplicates
    , defaultText = " "
    -- , autoComplete = Just 100000,   -- set Just 100000 for .1 sec
    , showCompletionOnTab = False -- False means auto completion
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    }

-- Set a window in centered floating mode.
-- TODO: Floating mode is always on top of all other windows.
toggleFloat :: Window -> X ()
toggleFloat w =
  windows
    (\s ->
       if M.member w (W.floating s)
         then W.sink w s
         else W.float w (W.RationalRect (1 / 4) (1 / 4) (1 / 2) (1 / 2)) s)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
  -- mod-button1, Set the window to floating mode and move by dragging
    [ ( (modm, button1)
      , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
  -- mod-button3, Set the window to floating mode and resize by dragging
    , ( (modm, button3)
      , \w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster)
  -- scroll the mouse wheel (button4 and button5)
    , ((modm, button4), \w -> focus w >> moveTo Prev nonNSP)
    , ((modm, button5), \w -> focus w >> moveTo Next nonNSP)
  -- drag windows
    , ((modm .|. shiftMask, button1), dragWindow)
    ]
  where
    nonNSP = ignoringWSs [scratchpadWorkspaceTag]

------------------------------------------------------------------------
-- All keybindings in this configuration
help :: String
help =
  intercalate "\" \""
    $ concatMap
        (\(x, y) -> [x, y])
        [ ("mod-Shift-Enter", "Launch " ++ myTerminal ++ " terminal")
        , ("mod-d", "Launch " ++ (head . words) myLauncher)
        , ("mod-space", "Bring up next available layout")
        , ("mod-shift-q", "Quit XMonad")
        , ("mod-? or mod-shift-/", "Open this help page")
        , ("mod-f", "Toggle focused window fullscreen (TODO)")
        , ("mod-g", "Toggle gaps")
        , ("mod-Enter", "Show/hide scratchpad")
        , ("mod-l", "Lock screen")
        , ("mod-x", "Open kanban board")
        , ("Screenshot chords\n---------------------------", "")
        , ("PrtSc", "Screenshots the entire workspace")
        , ("ctrl-PrtSc", "Screenshots focused application")
        , ("shift-PrtSc", "Interactively screenshot a rectangle")
        , ("Focus chords\n-------------------", "")
        , ("mod-c", "Close focused application")
        , ("mod-k", "Focus on previous window")
        , ("mod-l", "Focus on next window")
        , ("mod-s", "Swap focused window with master")
        , ("mod-tab", "Focus on next window")
        , ("Resizing chords\n-----------------------", "")
        , ("mod-shift-l", "Expand master area")
        , ("mod-shift-h", "Shrink master area")
        , ("mod-t", "Push floating window back to tiling")
        , ("mod-shift-t", "Push all floating windows back to tiling")
        , ("Workspace chords (TODO)\n---------------------------", "")
        , ("m-mod-[1..9]", "Switch to nth workspace")
        , ("m-mod-key", "change xinerama")
        , ("win-tab", "Switch back to last workspace")
        , ("Sound chords\n--------------------", "")
        , ("mod-a m", "Play/pause music")
        , ("mod-a n", "Forward to next track")
        , ("mod-a p", "Backward to previous track")
        , ("mod-a k", "Increase volume by 1%")
        , ("mod-a j", "Decrease volume by 1%")
        , ("mod-a l", "Mute volume")
        , ("Pomodoro chords\n---------------------", "")
        , ("mod-p n", "Start new 60 min session")
        , ("mod-p p", "Pause or resume an already running session")
        , ("mod-p s", "Skip this session or break")
        , ("Copy paste chords (TODO)\n---------------------------", "")
        , ("Notification chords\n----------------------------", "")
        , ("mod-n p", "Show previous notification")
        , ("mod-n c", "clear all notifications from screen")
        , ("mod-n shift-c", "delete all notifications from history")
        , ("Mouse bindings\n------------------------", "")
        , ("mod-leftmb", "Drag focused window to floating mode")
        , ("mod-rightmb", "Resize window by dragging")
        , ("mod-middlemb", "Move window to top of stack")
        , ("mod-scrollup", "Go to previous workspace")
        , ("mod-scrolldown", "Go to next workspace")
        , ("mod-shift-leftmb", "Drag window to another pane in this layout")
        ]
