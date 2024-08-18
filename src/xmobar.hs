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
    { font = "xft:Hack:size=8:antialias=true"
    , additionalFonts = ["xft:Material Icons:size=8:antialias=true"]
    , allDesktops = True
    , position = Static {xpos = 10, ypos = 1040, width = 1900, height = 25}
    , lowerOnStart = False
    , hideOnStart = False
    , pickBroadest = False
    , persistent = True
    , alpha = 100
    , fgColor = "white"
    , bgColor = "#0795e3"
    , commands =
        [ Run XMonadLog
        , Run
            $ Memory
                [ "-t"
                , "<fn=1>\xe322</fn> <usedratio>% <usedvbar>"
                , "--"
                , "scale"
                , "1024"
                ]
                10
        , Run HelloWorld
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
            $ Weather
                "VIDP"
                [ "-t"
                , "<station>: <tempC>°C <skyCondition>"
                , "-L"
                , "25"
                , "-H"
                , "40"
                , "--normal"
                , "green"
                , "--high"
                , "red"
                , "--low"
                , "lightblue"
                ]
                36000
        , Run
            $ Network
                "wlp3s0"
                ["-L", "0", "-H", "10240", "--normal", "red", "--high", "green"]
                10
        , Run
            $ MultiCpu
                [ "-t"
                , "<fn=1>\xe30d</fn> <total>% <vbar>"
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
                , "Temp: <avg>°C"
                , "-L"
                , "50"
                , "-H"
                , "85"
                , "-l"
                , "green"
                , "-n"
                , "yellow"
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
        , Run $ Date "%a %_d %b %Y <fc=yellow>%H:%M:%S</fc>" "date" 10
        ]
    , template =
        " %VIDP% | %wlp3s0% }{ %kbd% | %hw% | %date% | %multicpu% | %multicoretemp% | %memory% | %disku% | %battery% "
    , alignSep = "}{"
    }

main :: IO ()
main = xmobar config
