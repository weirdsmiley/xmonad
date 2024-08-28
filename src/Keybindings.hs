module Keybindings
  ( myXPConfig
  ) where

import qualified Data.Map as M
import Theme.Theme
import Workspaces
import XMonad
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.TiledWindowDragging (dragWindow)
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

-- -- These keybindings are for jumping to recently-used window instead of
-- -- moving in cycle.
-- myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
-- myKeys conf@XConfig {XMonad.modMask = modm} =
--   M.fromList
--     $ [ ((modm .|. controlMask, xK_Right), nextMatch Forward (className =? ""))
--       , ((modm .|. controlMask, xK_Left), nextMatch Backward (className =? ""))
--       ]
------------------------------------------------------------------------
-- XPrompt
--
myXPConfig :: XPConfig
myXPConfig =
  def
    { font = myFont
    , bgColor = basebg
    , fgColor = basefg
    , bgHLight = base04
    , fgHLight = base00
    , borderColor = base00
    , promptBorderWidth = 1
    , promptKeymap = defaultXPKeymap
    , position = Top
    -- , position = CenteredAt {xpCenterY = 0.3, xpWidth = 0.3}
    , alwaysHighlight = True -- Disables tab cycle
    , height = 30
    , maxComplRows = Just 10 -- set to 'Just 5' for 5 rows
    , historySize = 50
    , historyFilter = deleteAllDuplicates
    , defaultText = []
    -- , autoComplete = Just 100000,   -- set Just 100000 for .1 sec
    , showCompletionOnTab = False -- False means auto completion
    , searchPredicate = fuzzyMatch
    , sorter = fuzzySort
    }

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
