import Plugins.Bluetooth
import Plugins.Pomodoro
import Xmobar

config :: Config
config =
  defaultConfig
    { font = "xft:Fira Code Light:size=6:antialias=true"
    , additionalFonts =
        [ "xft:Font Awesome 6 Free-Regular-400:size=6:antialias=true"
        , "Material Icons:style=Regular"
        ]
    , allDesktops = True
    -- , position = Static {xpos = 10, ypos = 1050, width = 1900, height = 20}
    , position = Static {xpos = 10, ypos = 10, width = 1900, height = 30}
    , lowerOnStart = True
    , hideOnStart = False
    , pickBroadest = False
    , persistent = True
    , alpha = 110
    , fgColor = "white"
    , bgColor = "#5501c3"
    , commands =
        [ Run XMonadLog
        , Run $ PomodoroSession Start
        , Run
            $ Memory
                [ "-t"
                , "<fc=#ff79c6><fn=1>\xe322</fn></fc> <usedratio>% (<used>G)"
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
                [("/", "<fc=#ff79c6><fn=2>\xe1db</fn></fc> <free>/<size>")]
                ["-L", "50", "-H", "900", "-m", "1", "-p", "3"]
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
                , "<fn=2><skyConditionS></fn> <tempC>° <rh>% <windKmh>Km/h"
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
            $ Volume
                "default"
                "Master"
                [ "-t"
                , "<fc=#ff79c6><fn=2>\xe050</fn></fc> <volume>"
                , "-L"
                , "0"
                , "-M"
                , "40"
                , "-H"
                , "70"
                ]
                10
        , Run
            $ Network
                "wlp4s0"
                [ "-t"
                , "<fc=#ff79c6><fn=2>\xe63e</fn></fc> ↓<rx> ↑<tx>"
                , "-L"
                , "0"
                , "-H"
                , "10240"
                , "--normal"
                , "#ff79c6"
                , "--high"
                , "lightgreen"
                , "-S"
                , "True"
                ]
                10
        , Run
            $ MultiCpu
                [ "-t"
                , "<fc=#ff79c6><fn=1>\xf2db</fn></fc> <total>%"
                , "--Low"
                , "10" -- units: %
                , "--High"
                , "90" -- units: %
                , "--low"
                , "white"
                , "--normal"
                , "yellow"
                , "--high"
                , "lightred"
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
        , Run
            $ Com
                "/bin/sh"
                [ "-c"
                , "if [[ $(bluetoothctl show | awk -F': ' '/Powered:/ { print $2 }') == 'yes' ]]; then echo \"<fc=lightgreen><fn=2>\xe1a8</fn></fc>\"; else echo \"<fn=2>\xe1a9</fn>\"; fi"
                ]
                "bluetooth"
                1
        -- , Run $ Com "uname" ["-s", "-r"] "kernel" 0
        -- , Run $ Com "gnome-pomodoro" ["--start"] "pom" 0
        -- , Run
        --     $ CommandReader
        --         ("exec " <> "~/.config/xmonad/scripts/playerctl.sh")
        --         "playerctl"
        -- , Run
        --     $ Com "⏵" ["<action=`gnome-pomodoro` button=1>Pomodoro</action>"] 0
        ]
    , template =
        "<hspace=10/>%pomodoro%  %XMonadLog% }{ %wlp4s0% %bluetooth%  %default:Master%  %multicpu% %cpufreq% %multicoretemp%  %memory%  %disku%  %VIDP%  %date%<hspace=10/>"
    , alignSep = "}{"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
-- <hspace=10/><action=`gnome-pomodoro --start` button=1>⏵</action>
