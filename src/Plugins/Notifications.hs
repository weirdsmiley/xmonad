{-# LANGUAGE OverloadedStrings #-}

--- This plugin tracks whether notifications are enabled/disabled. Right mouse
--- click on the notification icon will read all notifications, left clicking
--- will toggle notification status, i.e. notifications will be enabled/disabled.
module Plugins.Notifications
  ( Notifications(..)
  ) where

import Preferences (myNotifHandler)
import System.Process (readProcess)
import Xmobar

data State
  = Active
  | Silent
  deriving (Eq, Show)

data Notifications =
  Notifications
  deriving (Read, Show)

iconActive :: String
iconActive = "<fn=3>\xeb9a</fn>"

iconSilent :: String
iconSilent = "<fn=3>\xec08</fn>"

instance Exec Notifications where
  alias _ = "notifications"
  rate _ = 10
  run _ = do
    state <- getState
    pure $ wrapAction (iconFor state)

iconFor :: State -> String
iconFor Active = iconActive
iconFor Silent = iconSilent

wrapAction :: String -> String
wrapAction icon =
  "<action=`"
    ++ myNotifHandler
    ++ " close-all` button=1><action=`"
    ++ myNotifHandler
    ++ " set-paused toggle` button=3>" <> icon <> "</action></action>"

silentModeAction :: String -> String
silentModeAction icon =
  "<action=`" ++ myNotifHandler ++ " set-paused toggle`>" <> icon <> "</action>"

getState :: IO State
getState = do
  out <- readProcess myNotifHandler ["is-paused"] ""
  pure
    $ case words out of
        ["true"] -> Silent
        _ -> Active
