#!/bin/bash

echo "Getting apt packages"
sudo apt-get -y install `cat ubuntu-pkgs`


# Emacs
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
emacs25

# TODO: passwd auth
# PIA install
sudo apt-get -y install network-manager-openvpn
pushd /tmp
wget https://www.privateinternetaccess.com/installer/pia-nm.sh
sudo bash pia-nm.sh
popd

# TODO: script dwm tools