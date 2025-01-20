import Plugins.Bluetooth
import Plugins.Pomodoro
import Plugins.Soundtrack
import Xmobar

config :: Config
config =
  defaultConfig
    { font = "Open Sans SemiBold 10"
    , additionalFonts =
        [ "Font Awesome 6 Free-Regular-400 10"
        , "Material Icons Regular 10"
        , "0xProto Nerd Font Regular 10"
        ]
    , allDesktops = True
    , position = Static {xpos = 10, ypos = 1040, width = 1900, height = 30}
    , border = NoBorder
    , lowerOnStart = False
    , hideOnStart = False
    , pickBroadest = False
    , persistent = False
    , alpha = 10
    , fgColor = "#f8f8f8"
    , bgColor = "#5501c3"
    , commands =
        [Run $ Soundtrack ["-t", "<fc=lightgreen><title> <artist></fc>"] 1000]
    , template = "} %soundtrack% {"
    , alignSep = "}{"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
