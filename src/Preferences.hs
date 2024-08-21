module Preferences
  ( 
  ) where

import Theme.Font
import Theme.Theme

-- Default terminal
myTerminal :: String
myTerminal = "kitty"

-- Utility function to get named terminals. These can be useful for auto
-- shifting windows based on title. For e.g., moving a scratchpad terminal to
-- the centre in floating mode can be done by modifying the manageHook as:
--    myScratchpad = myNamedTerminal "scratchpad"
--    myManageHook = composeOne [ title =? "scratchpad" -?> doCenterFloat ]
myNamedTerminal name = myTerminal ++ " --title " ++ name
