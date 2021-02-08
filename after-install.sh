!#/bin/bash

#Instalar o paru, um AUR helper substituto para o yay

sudo pacman -S xdg-user-dirs base-devel wget --noconfirm
cd ~/Downloads
git clone https://aur.archlinux.org/paru.git
cd paru
makepkg -si

sudo pacman -S alacritty nitrogen trayer network-manager-applet lxsession picom \
volumeicon nano thunar thunar-archive-plugin thunar-media-tags-plugin thunar-volman \
xfce4-clipman-plugin haskell-hinotify mpv qbittorrent firefox brave-bin papirus-icon-theme \
alsa-lib alsa-plugins pulseaudio-alsa pulseaudio terminus-font ark htop libreoffice-still \
discord dmenu bibata-cursor-theme arc-gtk-theme xmonad xmonad-contrib \
neofetch ttf-ubuntu-fonto-family ttf-hack cabal-install lightdm xterm --noconfirm

cabal update

cd ~/arch-monad
mkdir ~/.config/xmobar
mkdir ~/.xmonad
cp ./xmonadrc ~/.xmonad
cp ./xmobarrc ~/.config/xmobar/

sudo systemctl enable lightdm
