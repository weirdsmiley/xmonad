#!/bin/bash

# Configuration
BARS=8
FRAMERATE=60
CAVA_CONFIG="/tmp/xmobar_cava.conf"

# Generate a temporary cava config
cat > "$CAVA_CONFIG" <<EOF
[general]
bars = $BARS
framerate = $FRAMERATE

[output]
method = raw
raw_target = /dev/stdout
data_format = ascii
ascii_max_range = 7
EOF

# Unicode block characters corresponding to values 0-7
BAR_CHARS=" ▂▃▄▅▆▇█"
SED_DICT="s/;//g;"

# Build sed substitution dictionary
for ((i=0; i<8; i++)); do
    SED_DICT="${SED_DICT} s/$i/${BAR_CHARS:$i:1}/g;"
done

# Stream cava output unbuffered (-u) through the sed translator
cava -p "$CAVA_CONFIG" | sed -u "$SED_DICT"

