--- This plugin tracks tracks being played through myMusicCtrl. And provides
--- a user-interface to pause, play, forward and backward tracks.
module Plugins.Soundtrack
  ( Soundtrack(..)
  ) where

import Preferences (myMusicCtrl)
import System.Process
import Xmobar

-- This utility function is needed because myMusicCtrl returns a newline appended
-- string.
trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse

-- Returns the artist name.
getArtist :: IO String
getArtist = do
  artist <- readProcess myMusicCtrl ["metadata", "artist"] ""
  return $ takeWhile (/= ',') . trim $ artist

-- Returns the title track that is being played.
getTrack :: IO String
getTrack = do
  track <-
    readProcess myMusicCtrl ["metadata", "title"] "" >>= \x -> return (trim x)
  if length track > 14
    then return $ take 14 track ++ "..."
    else return track

-- Returns the album of track that is being played.
getAlbum :: IO String
getAlbum = do
  readProcess myMusicCtrl ["metadata", "album"] "" >>= \x -> return (trim x)

data Soundtrack =
  Soundtrack
  deriving (Show, Read)

instance Exec Soundtrack where
  alias Soundtrack = "soundtrack"
  run Soundtrack = do
    artist <- getArtist
    track <- getTrack
    _album <- getAlbum
    if null artist
      then return ""
      else return
             $ "<fc=lightgreen> "
                 ++ track
                 ++ " - "
                 ++ artist
                 ++ " "
                 ++ show Controller
                 ++ "</fc>"

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
