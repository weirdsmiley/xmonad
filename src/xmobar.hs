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
    { font = "xft:Hack Regular:size=6:antialias=true"
    , additionalFonts =
        ["xft:Font Awesome 6 Free Regular:size=6:antialias=true"]
    , allDesktops = True
    , position = Static {xpos = 10, ypos = 1050, width = 1900, height = 20}
    -- , position = Static {xpos = 10, ypos = 10, width = 1900, height = 30}
    , lowerOnStart = True
    , hideOnStart = False
    , pickBroadest = False
    , persistent = True
    , alpha = 110
    , fgColor = "white"
    , bgColor = "#5501c3"
    , commands =
        [ Run HelloWorld
        , Run XMonadLog
        , Run
            $ Memory
                [ "-t"
                , "<fn=1>\xe322</fn> <usedratio>% (<used>G)"
                -- , "<usedratio>% (<used> G)"
                , "-d"
                , "1"
                , "--"
                , "--scale"
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
                "wlp4s0"
                [ "-t"
                , "<dev>: <rx> | <tx>"
                , "-L"
                , "0"
                , "-H"
                , "10240"
                , "--normal"
                , "red"
                , "--high"
                , "green"
                , "-S"
                , "True"
                ]
                10
        , Run
            $ MultiCpu
                [ "-t"
                , "<fn=1>\xf2db</fn> <total>%"
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
        , Run $ Date "%a %_d %b %Y <fc=#ff79c6>%I:%M:%S %p</fc> %Z" "date" 10
        , Run $ Com "uname" ["-s", "-r"] "kernel" 0
        ]
    , template =
        " %XMonadLog% }{ %wlp4s0%  ▪  %VIDP%  ▪  %date%  ▪  %multicpu% %cpufreq% %multicoretemp%  ▪  %memory%  ▪  %disku% "
    , alignSep = "}{"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
