{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Layout
  ( myLayout
  ) where

import Preferences
import Theme.Theme (base00, base07, base08, basebg, basefg, myFont)
import XMonad
import XMonad.Actions.TiledWindowDragging (dragWindow)
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Accordion (Accordion(Accordion))
import XMonad.Layout.DraggingVisualizer (draggingVisualizer)
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiColumns (MultiCol, multiCol)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spacing
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation

------------------------------------------------------------------------
-- Tab theme
myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = base08
    , activeBorderColor = base08
    , activeTextColor = basefg
    , activeBorderWidth = 0
    , inactiveColor = base00
    , inactiveBorderColor = basebg
    , inactiveTextColor = base07
    , inactiveBorderWidth = 2
    , fontName = myFont
    , decoHeight = 30
    , decoWidth = maxBound
    }

myLayout =
  onWorkspace "1" (twoByThreeOnRight ||| twoByThreeOnLeft)
    $ onWorkspace "2" (multiColWithGaps ||| magnifiedMultiColWithGaps)
    $ onWorkspace "3" tiled
    $ mkToggle (NOBORDERS ?? NBFULL ?? EOT)
        . avoidStruts
        . lessBorders OnlyScreenFloat
    $ tiled
        ||| twoByThreeOnRight
        ||| twoByThreeOnLeft
        ||| Mirror tiled
        ||| tall
        ||| full
        ||| multiColWithGaps
  where
    multiColWithGaps = rn "Columns" . addGaps $ multiCol [1] 1 0.01 (-0.5)
    magnifiedMultiColWithGaps =
      rn "Columns" . addGaps $ magnifiercz' 2.0 $ multiCol [1] 1 0.01 (-0.5)
    full = rn "Full" . addGaps $ Full
    tall = rn "Tall" . addGaps $ ResizableTall nmaster delta ratio []
    threeCol = addGaps $ ThreeCol nmaster delta ratio
    twoByThreeOnRight =
      rn "2-by-3 (right)" . addGaps $ reflectHoriz $ Tall nmaster delta (2 / 3)
    twoByThreeOnLeft = rn "2-by-3 (left)" . addGaps $ Tall nmaster delta (2 / 3)
    tiled = rn "Tiled" . addGaps $ Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes
    mySpacing ::
         Integer -> l a -> ModifiedLayout XMonad.Layout.Spacing.Spacing l a
    mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True
    rn n = renamed [Replace n]
    mkTabbed layout =
      addTabs shrinkText myTabConfig . subLayout [] (Simplest ||| Accordion)
        $ layout
    dragWindows layout = windowNavigation . draggingVisualizer $ layout
    addGaps = mySpacing myGaps
-- myDecoratedLayout layout
--   -- onWorkspace "1" twoByThreeOnRight
--   --   $ onWorkspace "2" multiColWithGaps
--   --   $ onWorkspaces ["3", "4"] tiled $
--  =
--   mkToggle (NOBORDERS ?? NBFULL ?? EOT)
--     . avoidStruts
--     . lessBorders OnlyScreenFloat
--     $ layout
