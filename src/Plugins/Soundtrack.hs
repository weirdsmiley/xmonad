--- This plugin tracks tracks being played through myMusicCtrl. And provides
--- a user-interface to pause, play, forward and backward tracks.
module Plugins.Soundtrack
  ( Soundtrack(..)
  , getArtist
  , getRunningPlayer'
  ) where

import Data.List (find)
import Preferences (myMusicCtrl)
import System.Process
import XMonad.Core (X)
import XMonad.Util.Run (runProcessWithInput)
import Xmobar
import Xmobar.Plugins.Monitors.Common

-- This utility function is needed because myMusicCtrl returns a newline appended
-- string.
trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse

-- Returns the artist name.
getArtist :: IO String
getArtist = do
  player <- getRunningPlayer
  case player of
    Just p ->
      runProcessWithInput myMusicCtrl ["-p", p, "metadata", "artist"] "" >>= \x ->
        return (trim x)
    Nothing ->
      runProcessWithInput myMusicCtrl ["metadata", "artist"] "" >>= \x ->
        return (trim x)

-- Returns the title track that is being played.
getTrack :: IO String
getTrack = do
  player <- getRunningPlayer
  case player of
    Just p ->
      runProcessWithInput myMusicCtrl ["-p", p, "metadata", "title"] "" >>= \x ->
        return (trim x)
    Nothing ->
      runProcessWithInput myMusicCtrl ["metadata", "title"] "" >>= \x ->
        return (trim x)

-- Returns the album of track that is being played.
getAlbum :: IO String
getAlbum = do
  player <- getRunningPlayer
  case player of
    Just p ->
      runProcessWithInput myMusicCtrl ["-p", p, "metadata", "album"] "" >>= \x ->
        return (trim x)
    Nothing ->
      runProcessWithInput myMusicCtrl ["metadata", "album"] "" >>= \x ->
        return (trim x)

splitOn sep str = (before, after)
  where
    (before, _:after) = break (== sep) str

-- Returns the currently running player.
getRunningPlayer :: IO (Maybe String)
getRunningPlayer = do
  statuses <-
    runProcessWithInput
      myMusicCtrl
      ["--all-players", "-f", "\"{{playerName}}:{{status}}\"", "metadata"]
      "" >>= \x -> return (lines x)
  let runningPlayer = map (splitOn ':' . filter (/= '"')) statuses
  return $ fst <$> find (\(_, status) -> status == "Playing") runningPlayer

-- Same as getRunningPlayer but lifts IO for X (). This can be used directly in
-- spawn functions.
getRunningPlayer' :: X String
getRunningPlayer' = do
  statuses <-
    runProcessWithInput
      myMusicCtrl
      ["--all-players", "-f", "\"{{playerName}}:{{status}}\"", "metadata"]
      "" >>= \x -> return (lines x)
  let runningPlayer = map (splitOn ':' . filter (/= '"')) statuses
  return
    $ maybe "" fst (find (\(_, status) -> status == "Playing") runningPlayer)

data Soundtrack =
  Soundtrack Args Rate
  deriving (Show, Read)

soundtrackConfig :: IO MConfig
soundtrackConfig =
  mkMConfig "<title> <artist>" ["title", "artist", "album", "art"]

soundtrack :: Monitor [String]
soundtrack = do
  artist <- io getArtist
  track <- io getTrack
  album <- io getAlbum
  _u <- getConfigValue useSuffix -- This checks the configuration for using
                                 -- suffices (like d in 2d in uptime) but
                                 -- Soundtrack doesn't require suffices
                                 -- TODO: This can be useful for
                                 -- enabling/disabling separators when artist
                                 -- is not available.
  let str x =
        if null artist
          then ""
          else x
  mapM (`showWithColors'` 0) [str track, str artist, str album]

runSoundtrack :: [String] -> Monitor String
runSoundtrack _ = soundtrack >>= parseTemplate

instance Exec Soundtrack where
  alias (Soundtrack _ _) = "soundtrack"
  start (Soundtrack a r) = runM a soundtrackConfig runSoundtrack r

-- Soundtrack controller
data Controller =
  Controller
  deriving (Read)

play = " <action=`playerctl play`><fn=1>\xf04b</fn></action> "

pause = " <action=`playerctl pause`><fn=1>\xf04c</fn></action> "

forward = " <action=`playerctl next`><fn=1>\xf04e</fn></action> "

backward = " <action=`playerctl previous`><fn=1>\xf04a</fn></action> "

-- TODO: Move this into a Show instance and embed this controller directly in
-- Soundtrack.
instance Show Controller where
  show Controller = backward ++ play ++ pause ++ forward
