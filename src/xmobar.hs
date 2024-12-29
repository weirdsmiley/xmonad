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
        , Run Soundtrack
        , Run $ PomodoroSession Start
        , Run
            $ Memory
                [ "-t"
                , "<fc=#ff79c6><fn=2>\xf035b</fn></fc> <usedratio>% (<used>G)"
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
                , "lightblue"
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
                , "<action=`amixer set Master toggle` button=1><action=`amixer set Master 5%+` button=4><action=`amixer set Master 5%-` button=5><fc=#ff79c6><fn=2>\xf028 </fn></fc> <volume></action></action></action>"
                , "-L"
                , "0"
                , "-M"
                , "40"
                , "-H"
                , "70"
                ]
                10
        , Run
            $ DynNetwork
                [ "--template"
                , "<fc=#ff79c6><fn=2>\xf05a9</fn>  <dev></fc> ↓<rx> ↑<tx>"
                , "-L"
                , "0"
                , "-H"
                , "10240"
                , "--low"
                , "#ffcccb"
                , "--normal"
                , "#ff79c6"
                , "--high"
                , "lightgreen"
                , "-S"
                , "True"
                ]
                10
        , Run
            $ Network
                "enp2s0" -- Figure out dynamically TH?
                [ "-t"
                , "<fc=#ff79c6><fn=2>\xf05a9</fn></fc> ↓<rx> ↑<tx>"
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
                , "<fc=#ff79c6><fn=2>\xf4bc</fn></fc>  <total>%"
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
        , Run $ Date "%a %_d %b %Y <fc=#ff79c6>%I:%M:%S %p</fc> %Z" "date" 10
        , Run
            $ Com
                "/bin/sh"
                [ "-c"
                , "if [[ $(bluetoothctl show | awk -F': ' '/Powered:/ { print $2 }') == 'yes' ]]; then echo \"<fc=lightgreen><fn=2>\xe1a8</fn></fc>\"; else echo \"<fn=2>\xe1a9</fn>\"; fi"
                ]
                "bluetooth"
                1
        , Run
            $ Uptime ["-t", "<fc=#ff79c6><days> <hours></fc>", "-S", "True"] 10
        ]
    , template =
        "<hspace=10/>%pomodoro% %uptime%  %XMonadLog% }{ %dynnetwork% %bluetooth%  %default:Master%  %soundtrack% %multicpu% %cpufreq% %multicoretemp%  %memory%  %disku%  %VIDP%  %date%<hspace=10/>"
    , alignSep = "}{"
    }

main :: IO ()
main = configFromArgs config >>= xmobar
