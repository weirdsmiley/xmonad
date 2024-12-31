--- This plugin is an interface for gnome-pomodoro application to control
--- functionalities on xmobar using mouse actions.
module Plugins.Pomodoro
  ( PomodoroSession(..)
  , PomodoroStatus(..)
  ) where

import Xmobar

-- Store a status and a timer countdown (3-size tuple of ints)
-- on the first click at 〇, start timer and put status as start
-- on the second click
data PomodoroSession =
  PomodoroSession PomodoroStatus
  deriving (Show, Read)

instance Exec PomodoroSession where
  alias (PomodoroSession _) = "pomodoro"
  run (PomodoroSession status) = sessionCmds status

-- If status is start, then 
-- run (PomodoroSession status) = 
data PomodoroStatus
  = Start
  | Pause
  | Resume
  | Stop
  deriving (Show, Read)

sessionAliases :: PomodoroStatus -> String
sessionAliases Start = "start"
sessionAliases Pause = "pause"
sessionAliases Resume = "resume"
sessionAliases Stop = "stop"

sessionCmds :: PomodoroStatus -> IO String
sessionCmds Start =
  return
    "<action=`dunstify 'Pomodoro' 'Toggled State'; gnome-pomodoro --pause-resume` button=1><fn=1><fc=#ff79c6>\xf017</fc></fn></action>"
sessionCmds Pause =
  return
    "<action=`gnome-pomodoro --resume` button=1><fc=#ff79c6>⏸</fc></action>"
sessionCmds Resume = return "<fc=red>⏵</fc>"
sessionCmds Stop = return "<fc=red>〇</fc>"
