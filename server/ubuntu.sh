#!/bin/bash

# special setup stuff for ubuntu

echo "Getting apt packages"
sudo apt-get -y install `cat ubuntu-pkgs`


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
    sudo apt-add-repository -y ppa:system76-dev/stable
    sudo apt update
    sudo apt full-upgrade
    sudo apt-get -y install system76-driver
fi

# install java 8
sudo add-apt-repository -y ppa:webupd8team/java
sudo apt-get update
# auto signs the license
echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | sudo /usr/bin/debconf-set-selections
sudo apt-get -y install oracle-java8-installer
sudo apt-get -y install oracle-java8-set-default

# add latest erlang
sudo apt-get -y install erlang
