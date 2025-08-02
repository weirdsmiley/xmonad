--- This plugin is a timer for managing pomodoro sessions. It runs a timer and
--- shows it on xmobar.
--- To interact with pomodoro via cmdline:
---    echo "start" > /tmp/pomodoro.ctrl
---    echo "skip" > /tmp/pomodoro.ctrl
---    echo "toggle" > /tmp/pomodoro.ctrl
module Plugins.Pomodoro
  ( Pomodoro(..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently_)
import Control.Monad (forever, when)
import Data.IORef
import Data.Time
import Data.Time.Clock
import Data.Time.Format
import System.Directory (doesFileExist, removeFile)
import System.IO (readFile, writeFile)
import Xmobar
import Xmobar.Plugins.Monitors.Common

data Mode
  = Work
  | ShortBreak
  | LongBreak
  deriving (Eq, Show)

data Pomodoro = Pomodoro
  { workMinutes :: Int
  , shortBreakMinutes :: Int
  , longBreakMinutes :: Int
  , controlFile :: FilePath -- e.g., "/tmp/pomodoro.ctrl"
  } deriving (Read, Show)

instance Exec Pomodoro where
  alias _ = "pomodoro"
  start (Pomodoro w s l ctrlPath) cb = do
    let workSecs = w * 60
        shortSecs = s * 60
        longSecs = l * 60
    pomodoroLoop 0 Work workSecs workSecs shortSecs longSecs True cb ctrlPath

pomodoroLoop ::
     Int -- ^ Pomodoro session count
  -> Mode -- ^ Current mode
  -> Int -- ^ Duration of session (seconds)
  -> Int -- ^ Work duration
  -> Int -- ^ Short break duration
  -> Int -- ^ Long break duration
  -> Bool -- ^ Paused state
  -> (String -> IO ()) -- ^ Xmobar callback
  -> FilePath -- ^ Control file path
  -> IO ()
pomodoroLoop count mode duration w s l paused cb ctrlPath = do
  let loop timeLeft pausedNow = do
        action <- readControl ctrlPath
        let newPaused =
              case action of
                Just "toggle" -> not pausedNow
                _ -> pausedNow
        case action of
          Just "skip" -> transition
          Just "start" -> pomodoroLoop 0 Work w w s l False cb ctrlPath
          _ ->
            if newPaused
              then do
                cb (render mode timeLeft ++ " <fc=#fa3b00>[PAUSED]</fc>")
                threadDelay 1000000
                loop timeLeft newPaused
              else do
                let newTimeLeft = timeLeft - 1
                if newTimeLeft >= 0
                  then do
                    cb (render mode newTimeLeft)
                    threadDelay 1000000
                    loop newTimeLeft newPaused
                  else transition
      transition = do
        let nextMode =
              case mode of
                Work ->
                  if (count + 1) `mod` 4 == 0
                    then LongBreak
                    else ShortBreak
                _ -> Work
            nextDuration =
              case nextMode of
                Work -> w
                ShortBreak -> s
                LongBreak -> l
            nextCount =
              if mode == Work
                then count + 1
                else count
        pomodoroLoop nextCount nextMode nextDuration w s l False cb ctrlPath
  loop duration paused

readControl :: FilePath -> IO (Maybe String)
readControl path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      removeFile path
      pure (Just $ takeWhile (/= '\n') content)
    else pure Nothing

-- Render output for Xmobar
render :: Mode -> Int -> String
render mode secsLeft =
  let (m, s) = secsLeft `divMod` 60
      icon =
        case mode of
          Work -> "⏱"
          ShortBreak -> "☕"
          LongBreak -> "🛌"
      label =
        case mode of
          Work -> "<fc=#92ff00>[Work]</fc>"
          ShortBreak -> "<fc=#bababa>[Break]</fc>"
          LongBreak -> "<fc=#ab7bab>[Long]</fc>"
   in icon ++ " " ++ pad m ++ ":" ++ pad s ++ " " ++ label

pad :: Int -> String
pad n =
  if n < 10
    then '0' : show n
    else show n
