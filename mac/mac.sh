#!/bin/bash

# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

### Install pkgs with brew
brew install `cat mac/osx-pkgs`

### Install normal packages
INSTALL_DIR="~/Downloads/mac-install"
mkdir -p $INSTALL_DIR > /dev/null 2>&1

function get-pkg () {
    pushd $INSTALL_DIR
    ZIP_FILE="${1}.gz"
    wget "${2}" -O $ZIP_FILE
    unzip $ZIP_FILE
    popd
}

# VSCode
get-pkg vscode "https://go.microsoft.com/fwlink/?LinkID=620882"

# download packages (dmg/pkg files)
pushd $INSTALL_DIR
wget https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg
wget https://pqrs.org/osx/karabiner/files/Karabiner-Elements-12.0.0.dmg
wget https://nodejs.org/dist/v8.11.2/node-v8.11.2.pkg
wget https://download-installer.cdn.mozilla.net/pub/devedition/releases/61.0b5/linux-x86_64/en-US/firefox-61.0b5.tar.bz2
tar -xfj firefox-61.0b5.tar.bz2
popd

# move karabiner config
mv mac/karabiner.json ~/.config/karabiner/assets/ctrl_esc.json

# post-install reminder
mv mac/osx-install-todo.org ~/