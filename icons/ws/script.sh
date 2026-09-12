#!/bin/bash

# Path to your working font (update if needed)
FONT="/usr/share/fonts/cantarell/Cantarell-VF.otf"

# Loop from 1 to 10
for i in {1..10}; do
    magick -size 100x100 xc:none \
        -fill white -stroke black -strokewidth 2 \
        -draw "circle 50,50 50,5" \
        -gravity center -fill black -font "$FONT" -weight 800 -pointsize 85 \
        -annotate +0+0 "$i" "${i}.xpm"

    # Resize down to height 10 maintaining aspect ratio
    magick "${i}.xpm" -resize x14 "${i}.xpm"
    echo "Created: ${i}.xpm"
done

echo "All 10 bold circular XPM images generated successfully!"

magick -size 100x100 xc:none \
    -fill "#808080" -stroke white -strokewidth 16 \
    -draw "circle 50,50 50,13" \
    -gravity center -fill black -font "$FONT" -weight 800 -pointsize 85 \
    "focused.xpm"
magick "focused.xpm" -resize x14 "focused.xpm"
