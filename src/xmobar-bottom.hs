import Plugins.Soundtrack
import Theme.Font (myXmobarFont)
import Xmobar

config :: Config
config =
  defaultConfig
    { font = myXmobarFont
    , additionalFonts =
        [ "Font Awesome 6 Free-Regular-400 10"
        , "Material Icons Regular 10"
        , "0xProto Nerd Font Regular 10"
        , "Open Sans Bold 10"
        ]
    , allDesktops = True
    , position = Static {xpos = 10, ypos = 1050, width = 1900, height = 30}
    , border = NoBorder
    , lowerOnStart = True
    , hideOnStart = False
    , pickBroadest = False
    , persistent = True
    , alpha = 0
    , fgColor = "#f8f8f8"
    , bgColor = "#5501c3"
    , commands =
        [ Run
            $ Soundtrack
                [ "-t"
                , "<fn=4><fc=lightgreen><title> - <album> - <artist></fc></fn>"
                ]
                1
        , Run
            $ Memory
                [ "-t"
                , "<fc=#ff79c6><fn=2>\xf035b</fn></fc>  <usedratio>% (<used>G)"
                , "-d"
                , "1"
                , "--"
                , "--scale"
                , "1024"
                ]
                10
        , Run
            $ DiskU
                [("/", "<fc=#ff79c6><fn=1>\xf1c0</fn></fc> <free>/<size>")]
                ["-L", "50", "-H", "900", "-m", "1", "-p", "3"]
                20
        , Run
            $ MultiCpu
                [ "-t"
                , "<fc=#ff79c6><fn=2>\xf4bc</fn></fc>   <total>%"
                , "--Low"
                , "10" -- units: %
                , "--High"
                , "90" -- units: %
                , "--low"
                , "white"
                , "--normal"
                , "yellow"
                , "--high"
                , "#ffcccb"
                ]
                20
        , Run
            $ MultiCoreTemp
                [ "-t"
                , "<avg>°C"
                , "-L"
                , "40"
                , "-H"
                , "85"
                , "-l"
                , "#21fdff"
                , "-n"
                , "white"
                , "-h"
                , "red"
                , "--"
                , "--mintemp"
                , "20"
                , "--maxtemp"
                , "100"
                ]
                50
        , Run $ Kbd []
        , Run
            $ CpuFreq
                [ "-t"
                , "<cpu0> GHz"
                , "-L"
                , "0"
                , "-H"
                , "5"
                , "-l"
                , "lightblue"
                , "-n"
                , "white"
                , "-h"
                , "orange"
                ]
                50
        ]
    , template =
        "<hspace=10/>} %soundtrack% { %multicpu% %cpufreq% %multicoretemp%  %memory%  %disku% <hspace=10/>"
    , alignSep = "}{"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
