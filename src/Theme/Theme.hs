{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module : Theme.Theme
   Copyright : (c) 2021 Joan Milev <joantmilev@gmail.com>
   License : MIT

   Maintainer : Joan Milev <joantmilev@gmail.com>
   Stability : Stable
   Portability : Unknown
-}
module Theme.Theme
  ( basebg
  , basefg
  , basecr
  , base00
  , base08
  , base01
  , base09
  , base02
  , base0A
  , base03
  , base0B
  , base04
  , base0C
  , base05
  , base0D
  , base06
  , base0E
  , base07
  , base0F
  , topBarTheme
  , myVisualSubmapDef
  ) where

import Prelude (String)
import Theme.Font
import Theme.Xresources (xprop)
import XMonad.Layout.NoFrillsDecoration
import qualified XMonad.Util.XUtils as XU

basebg, basefg, basecr, base00, base08, base01, base09, base02, base0A, base03, base0B, base04, base0C, base05, base0D, base06, base0E, base07, base0F ::
     String
basebg = xprop "*.background"

basefg = xprop "*.foreground"

basecr = xprop "*.cursorColor"

base00 = xprop "*.color0"

base08 = xprop "*.color8"

base01 = xprop "*.color1"

base09 = xprop "*.color9"

base02 = xprop "*.color2"

base0A = xprop "*.color10"

base03 = xprop "*.color3"

base0B = xprop "*.color11"

base04 = xprop "*.color4"

base0C = xprop "*.color12"

base05 = xprop "*.color5"

base0D = xprop "*.color13"

base06 = xprop "*.color6"

base0E = xprop "*.color14"

base07 = xprop "*.color7"

base0F = xprop "*.color15"

------------------------------------------------------------------------
-- this is a "fake title" used as a highlight bar in lieu of full borders
-- (I find this a cleaner and less visually intrusive solution)
-- ack:
-- https://github.com/altercation/dotfiles-tilingwm/blob/31e23a75eebdedbc4336e7826800586617d7d27d/.xmonad/xmonad.hs#L519
topBarTheme =
  def
    { fontName = "xft:Hack:style=Bold:pixelsize=9"
    , inactiveBorderColor = basebg
    , inactiveColor = basebg
    , inactiveTextColor = base03
    , activeBorderColor = base04
    , activeColor = base04
    , activeTextColor = basebg
    , urgentBorderColor = base01
    , urgentTextColor = base03
    , decoHeight = 10
    }

myVisualSubmapDef =
  def
    { XU.winFont = "xft:monospace-20"
    , XU.winFg = "#C7B9B9"
    , XU.winBg = "#42372D"
    }
