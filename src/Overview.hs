module Overview
  ( myOverview
  , myWindowOverview
  ) where

import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowBringer
import qualified XMonad.StackSet as W

------------------------------------------------------------------------
-- Grid select UI
myColorizer :: Window -> Bool -> X (String, String)
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

myOverview :: X ()
myOverview = goToSelected myGridConfig

myWindowOverview :: X ()
myWindowOverview = gotoMenuArgs def
