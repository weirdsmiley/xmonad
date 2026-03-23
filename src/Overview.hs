module Overview
  ( myOverview
  , myWindowOverview
  , myWorkspaceOverview
  ) where

import Control.Monad (filterM, forM)
import qualified Data.Map as M
import Data.Maybe (listToMaybe)
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W
import XMonad.Util.NamedWindows (getName)

workspaceLabel :: WorkspaceId -> WindowSpace -> X String
workspaceLabel ws wspace =
  case W.stack wspace of
    Nothing -> return (ws ++ " No windows")
    Just st -> do
      -- let win = W.focus st
      -- name <- getName win
      let wins = W.integrate st
      names <- mapM getName wins
      return (ws ++ " " ++ unwords (map show names) ++ " | ")

workspaceMap :: X [(String, WorkspaceId)]
workspaceMap = do
  ws <- gets windowset
  let wspaces = W.workspaces ws
  mapM
    (\w -> do
       label <- workspaceLabel (W.tag w) w
       return (label, W.tag w))
    wspaces

-- myColorizer w active = do
--   cls <- runQuery className w
--   let base =
--         case cls of
--           "firefox" -> "#bf616a"
--           "kitty" -> "#a3be8c"
--           _ -> "#4c566a"
--   return
--     $ if active
--         then ("#ffffff", base)
--         else ("#d8dee9", "#2e3440")
myColorizer _ active =
  return
    ( if active
        then "#ffffff"
        else "#a6a6a6"
    , "#1c1f24")

myGridConfig :: GSConfig Window
myGridConfig =
  def
    { gs_cellheight = 200
    , gs_cellwidth = 250
    , gs_cellpadding = 20
    , gs_font = "xft:Open Sans SemiBold:size=10"
    , gs_navigate = navNSearch
    , gs_colorizer = myColorizer
    }

myGSConfig :: GSConfig WorkspaceId
myGSConfig =
  def
    { gs_cellheight = 180
    , gs_cellwidth = 400
    , gs_cellpadding = 20
    , gs_font = "xft:Open Sans SemiBold:size=10"
    , gs_navigate = navNSearch
    -- , gs_colorizer = myColorizer
    }

notScratchpad :: Window -> X Bool
notScratchpad w = do
  ws <- gets windowset
  let scratchpadWs = filter (\x -> W.tag x == "NSP") (W.workspaces ws)
      scratchpadWins = concatMap (maybe [] W.integrate . W.stack) scratchpadWs
  return $ w `notElem` scratchpadWins

myOverview :: X ()
myOverview = goToSelected myGridConfig

myWindowOverview :: X ()
myWindowOverview = gotoMenuArgs def

myWorkspaceOverview :: X ()
myWorkspaceOverview = do
  ws <- workspaceMap
  gridselect myGSConfig ws >>= flip whenJust (windows . W.greedyView)
