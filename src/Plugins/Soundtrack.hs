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
  return $ trim artist

-- Returns the title track that is being played.
getTrack :: IO String
getTrack = do
  title <- readProcess "playerctl" ["metadata", "title"] ""
  return $ trim title

data Soundtrack =
  Soundtrack
  deriving (Show, Read)

instance Exec Soundtrack where
  alias Soundtrack = "soundtrack"
  run Soundtrack = do
    artist <- getArtist
    track <- getTrack
    return $ track ++ " - " ++ artist
