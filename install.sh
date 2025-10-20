#!/usr/bin/bash

## Installation script

DISTRO=$(cat /etc/*-release | grep 'ID=' | awk -F'=' 'NR==1 { print $2 }')

cp ~/.config/xmonad/.Xresources ~/.Xresources

# Install appropriate fonts
if [ $DISTRO == "fedora" ]; then
  sudo dnf install \
    google-noto-sans-fonts \
    fira-code-fonts \
    fontawesome-6-free-fonts \
    alsa-utils libXScrnSaver libXft libXpm cairo pango \
    dmenu rofi feh
elif [ $DISTRO == "artix" ] || [ $DISTRO == "arch" ]; then
  sudo pacman -S noto-fonts \
    ttf-fira-code \
    ttf-font-awesome \
    alsa-utils libxss libxft libxpm cairo pango \
    dmenu rofi feh
elif [ $DISTRO == "gentoo" ]; then
  sudo emerge media-fonts/noto \
    media-fonts/fira-code \
    media-fonts/fontawesome \
    media-sound/alsa-utils x11-libs/libXScrnSaver \
    x11-libs/libXft x11-libs/libXpm \
    x11-libs/cairo x11-libs/pango \
    x11-misc/dmenu x11-misc/rofi media-gfx/feh
else
cat << EOF
Automated font installation isn't supported for $DISTRO.
Please try manually installing the following fonts:
  1. Noto Sans
  2. Fira Code
  3. Font Awesome 6 Free
  4. 0xProto Nerd Font
EOF
fi

echo "exec xmonad" > ~/.xinitrc
echo "exec xmobar" >> ~/.xinitrc
