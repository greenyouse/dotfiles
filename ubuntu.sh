#!/bin/bash

# special setup stuff for ubuntu

echo "Getting apt packages"
sudo apt install `cat extra-packages.txt`


# install google-chrome
mkdir ~/local && cd ~/local
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
# fixes a dependency error
sudo apt install -f

# get the lastest firefox dev
wget https://download.mozilla.org/?product=firefox-aurora-latest-ssl&os=linux64&lang=en-US
# write a startup script for ff-dev
cat << EOF > ~/bin/firefox
#!/bin/bash

. ~/local/firefox/firefox
EOF


# one computer needs a driver for the screen
if [[ `lspci | grep Skylake` ]]; then
    sudo apt-add-repository ppa:system76-dev/stable
    sudo apt update
    sudo apt full-upgrade
    sudo apt install system76-driver
fi
