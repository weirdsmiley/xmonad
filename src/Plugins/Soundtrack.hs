--- This plugin tracks tracks being played through playerctl tool. And provides
--- a user-interface to pause, play, forward and backward tracks.
module Plugins.Soundtrack
  ( getArtist
  , getTrack
  , Soundtrack(..)
  ) where

import System.Process
import Xmobar

-- This utility function is needed because playerctl returns a newline appended
-- string.
trim :: String -> String
trim = reverse . dropWhile (== '\n') . reverse

-- Returns the artist name.
getArtist :: IO String
getArtist = do
  artist <- readProcess "playerctl" ["metadata", "artist"] ""
  return $ takeWhile (/= ',') . trim $ artist

-- Returns the title track that is being played.
getTrack :: IO String
getTrack = do
  track <-
    readProcess "playerctl" ["metadata", "title"] "" >>= \x -> return (trim x)
  if length track > 14
    then return $ take 14 track ++ "..."
    else return track

data Soundtrack =
  Soundtrack
  deriving (Show, Read)

instance Exec Soundtrack where
  alias Soundtrack = "soundtrack"
  run Soundtrack = do
    artist <- getArtist
    track <- getTrack
    if length artist == 0
      then return $ ""
      else return $ track ++ " - " ++ artist
