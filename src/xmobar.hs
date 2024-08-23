import Xmobar

data HelloWorld =
  HelloWorld
  deriving (Show, Read)

instance Exec HelloWorld where
  alias HelloWorld = "hw"
  run HelloWorld = return "<fc=red>Hello World!</fc>"

config :: Config
config =
  defaultConfig
    { font = "xft:Hack:size=6:antialias=true"
    , additionalFonts = ["xft:Material Icons:size=6:antialias=true"]
    , allDesktops = True
    , position = Static {xpos = 10, ypos = 1050, width = 1900, height = 20}
    , lowerOnStart = True
    , hideOnStart = False
    , pickBroadest = False
    , persistent = True
    , alpha = 255
    , fgColor = "white"
    , bgColor = "#282828"
    , commands =
        [ Run HelloWorld
        , Run XMonadLog
        , Run
            $ Memory
                [ "-t"
                , "<fn=1>\xe322</fn> <usedratio>% <usedvbar>"
                , "--"
                , "scale"
                , "1024"
                ]
                10
        , Run
            $ DiskU
                [("/", "<free>/<size>")]
                ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]
                20
        , Run
            $ Battery
                [ "-t"
                , "<acstatus>"
                , "--Low"
                , "30"
                , "--High"
                , "90"
                , "--low"
                , "red"
                , "--normal"
                , "yellow"
                , "--high"
                , "green"
                , "--"
                , "-o"
                , "<fn=1>\xe1a4</fn> <left>% (<timeleft>)"
                , "-O"
                , "<fn=1>\xe1a3</fn>"
                , "-i"
                , "<fn=1>\xe1a4</fn>"
                ]
                50
        , Run
            $ WeatherX
                "VIDP"
                [ ("clear", "🌣")
                , ("sunny", "🌣")
                , ("mostly clear", "🌤")
                , ("mostly sunny", "🌤")
                , ("partly sunny", "⛅")
                , ("fair", "🌑")
                , ("cloudy", "☁")
                , ("overcast", "☁")
                , ("partly cloudy", "⛅")
                , ("mostly cloudy", "🌧")
                , ("considerable cloudiness", "⛈")
                ]
                [ "-t"
                , "<fn=2><skyConditionS></fn> <tempC>° <rh>%  <windKmh> (<hour>)"
                , "-L"
                , "10"
                , "-H"
                , "25"
                , "--normal"
                , "black"
                , "--high"
                , "#ffd63a"
                , "--low"
                , "#21fdff"
                ]
                18000
        , Run
            $ Network
                "wlp3s0"
                ["-L", "0", "-H", "10240", "--normal", "red", "--high", "green"]
                10
        , Run
            $ MultiCpu
                [ "-t"
                , "<fn=1>\xe30d</fn> <total>%"
                , "--Low"
                , "10" -- units: %
                , "--High"
                , "85" -- units: %
                , "--low"
                , "white"
                , "--normal"
                , "yellow"
                , "--high"
                , "red"
                ]
                20
        -- cpu core temperature monitor
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
        , Run $ Date "%a %_d %b %Y <fc=white>%H:%M:%S</fc>" "date" 10
        ]
    , template =
        " %XMonadLog% }{ %kbd% ▪ %VIDP% ▪ %date% ▪ %multicpu% %cpufreq% %multicoretemp% ▪ %memory% ▪ %disku% "
    , alignSep = "}{"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
