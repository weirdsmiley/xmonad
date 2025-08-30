module Theme.Dmenu
  ( myDmenu
  ) where

import XMonad (X)
import XMonad.Hooks.DynamicLog (trim)
import XMonad.Util.Run (runProcessWithInput)

-- Define a customized dmenu function with arguments
myDmenu :: [String] -> X String
myDmenu choices = dmenuArgs dmenu_cmd choices
  where
    dmenu_cmd = "dmenu"
    dmenu_args =
      [ "-b"
      , "-p"
      , "Command"
      , "-fn"
      , "Fira Code SemiBold:pixelsize=14"
      , "-nb"
      , "#282a36"
      , "-nf"
      , "#f8f8f2"
      , "-sb"
      , "#bd93f9"
      , "-sf"
      , "#f8f8f2"
      ]
    dmenuArgs _ _ = do
      result <- runProcessWithInput "dmenu" dmenu_args (unlines choices)
      return (trim result)
