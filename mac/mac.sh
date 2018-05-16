#!/bin/bash

# install homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

function brew-install () {
    brew install "${1}"
}

### Install pkgs with brew
brew install `cat osx-pkgs`

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

# github desktop
get-pkg github-desktop "https://central.github.com/deployments/desktop/desktop/latest/darwin"

# VSCode
get-pkg vscode "https://go.microsoft.com/fwlink/?LinkID=620882"

# download browsers (dmg files)
pushd $INSTALL_DIR
wget https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg
https://download.mozilla.org/?product=firefox-aurora-latest-ssl&os=osx&lang=en-US
popd