--- This plugin tracks tracks being played through myMusicCtrl. And provides
--- a user-interface to pause, play, forward and backward tracks.
module Plugins.Soundtrack
  ( Soundtrack(..)
  , getArtist
  ) where

import Preferences (myMusicCtrl)
import System.Process
import Xmobar
import Xmobar.Plugins.Monitors.Common

logLength = 14

-- This utility function is needed because myMusicCtrl returns a newline appended
-- string.
trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse

-- Returns the artist name.
getArtist :: IO String
getArtist = do
  readProcess myMusicCtrl ["metadata", "artist"] "" >>= \x -> return (trim x)

-- Returns the title track that is being played.
getTrack :: IO String
getTrack = do
  readProcess myMusicCtrl ["metadata", "title"] "" >>= \x -> return (trim x)

-- Returns the album of track that is being played.
getAlbum :: IO String
getAlbum = do
  readProcess myMusicCtrl ["metadata", "album"] "" >>= \x -> return (trim x)

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
  u <- getConfigValue useSuffix -- TODO: What does this do?
  let str x =
        if null artist
          then ""
          else x
  mapM (`showWithColors'` 0) [str track, str artist, str album]

runSoundtrack :: [String] -> Monitor String
runSoundtrack _ = soundtrack >>= parseTemplate

instance Exec Soundtrack where
  alias (Soundtrack _ _) = "soundtrack"
  start (Soundtrack args rate) = runM args soundtrackConfig runSoundtrack rate

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
