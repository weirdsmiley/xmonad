#!/usr/bin/bash

## Installation script

DISTRO=$(cat /etc/*-release | grep 'ID=' | awk -F'=' 'NR==1 { print $2 }')

# Install appropriate fonts
if [ $DISTRO == "fedora" ]; then
  sudo dnf install \
    google-noto-sans-fonts \
    fira-code-fonts \
    fontawesome-6-free-fonts
elif [ $DISTRO == "artix" ] || [ $DISTRO == "arch" ]; then
  sudo pacman -S noto-fonts \
    ttf-fira-code \
    ttf-font-awesome
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

# Install configurations to ~/.config
cp -r ../xmonad/ ~/.config/

# TODO: provide wallpapers in repo

echo "exec xmonad" > ~/.xinitrc
echo "exec xmobar" >> ~/.xinitrc
