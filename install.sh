#!/usr/bin/bash

## Installation script

# For backup, append a random string as extension. Uses the same string for all
# backups for one iteration.
EXT=$(head -c 4 /dev/urandom | base64 | tr -dc 'a-zA-Z')

make_backup() {
  if [ -e "$1" ]; then
    mv $1 $1.$EXT
  fi
}

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
if [ -d "~/.config/xmonad/" ]; then
  make_backup ~/.config/xmonad
fi
cp -r ../xmonad/ ~/.config/

# TODO: provide wallpapers in repo

if [ -e "~/.xinitrc" ]; then
  make_backup ~/.xinitrc
fi
echo "exec xmonad" >> ~/.xinitrc
echo "exec xmobar" >> ~/.xinitrc
