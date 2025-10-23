#!/usr/bin/bash

## Installation script

DISTRO=$(cat /etc/*-release | grep 'ID=' | awk -F'=' 'NR==1 { print $2 }')

cp ~/.config/xmonad/.Xresources ~/.Xresources

# Install appropriate fonts
if [ $DISTRO == "fedora" ]; then
  sudo dnf install --skip-unavailable \
    google-noto-sans-fonts fira-code-fonts fontawesome-6-free-fonts \
    open-sans-fonts la-capitaine-cursor-theme \
    alsa-utils libXScrnSaver libXft libXft-devel libXpm libXpm-devel \
    alsa-lib-devel libX11-devel libXrandr-devel libXScrnSaver-devel \
    cairo cairo-devel pango pango-devel \
    dmenu rofi feh kitty yad unclutter-xfixes
elif [ $DISTRO == "artix" ] || [ $DISTRO == "arch" ]; then
  sudo pacman -S --noconfirm \
    noto-fonts ttf-fira-code ttf-font-awesome ttf-opensans \
    capitaine-cursors alsa-utils libxss libxft libxpm cairo pango \
    dmenu rofi feh kitty yad unclutter
elif [ $DISTRO == "gentoo" ]; then
  sudo emerge media-fonts/noto \
    media-fonts/fira-code \
    media-fonts/fontawesome \
    media-fonts/open-sans \
    media-sound/alsa-utils x11-libs/libXScrnSaver \
    x11-libs/libXft x11-libs/libXpm \
    x11-libs/cairo x11-libs/pango \
    x11-misc/dmenu x11-misc/rofi media-gfx/feh x11-terms/kitty \
    x11-misc/unclutter-xfixes
else
cat << EOF
Automated font installation isn't supported for $DISTRO.
Please try manually installing the following fonts:
  1. Noto Sans
  2. Fira Code
  3. Font Awesome 6 Free
  4. 0xProto Nerd Font
  5. Open Sans
EOF
fi

echo "exec xmonad" > ~/.xinitrc
echo "exec xmobar" >> ~/.xinitrc

stack install
