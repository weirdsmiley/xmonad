module Theme.Font
  ( myFont
  , myFontGTK
  , myBigFont
  , myBoldFont
  , myItalicFont
  , myXmobarFont
  ) where

import Theme.Xresources (xprop)

myFont, myFontGTK, myBigFont, myBoldFont, myItalicFont, myXmobarFont :: String
myFont = xprop "xmonad.font"

myFontGTK = xprop "xmonad.font.gtk"

myBigFont = xprop "xmonad.font.big"

myBoldFont = xprop "xmonad.font.bold"

myItalicFont = xprop "xmonad.font.italic"

myXmobarFont = xprop "xmobar.font"
