# dotfiles

Here are some configs that I've been using for a while.

My primary setup is Ubuntu but this should work across other distros and
systems like OSX or BSDs.

To get rolling take a look at the
[woot](https://github.com/greenyouse/dotfiles/blob/woot)
script which handles installs and backups. I chose to use the
[nix pkg manager](https://nixos.org/nix/) for most of the tools I
use since it works well cross platform and has
declarative/immutable/static build awesomeness. The list of nix
pkgs is in [packages.nix](https://github.com/greenyouse/dotfiles/blob/packages.nix).

For any packages not supported by `nix` you can use your native package
manager and swap out `apt-get` in the woot script for whatever you'll use. I made
[extra-packages.txt](https://github.com/greenyouse/dotfiles/blob/extra-packages.txt)
for any extra packages and you should tweak both of the packages files to your liking.

## Install

The install should be something like:

```sh
git clone https://github.com/greenyouse/dotfiles
mv dotfiles ~/.config
cd ~/.config/dotfiles
# assumes your $HOME is clear of dotfiles like .bashrc or .profile
./woot install
```

Backups go like this:

```sh
cd ~/.config/dotfiles
./woot backup storage-drive/backup-folder
```

Uninstall:
```sh
# this deletes nix too
./woot uninstall
```


## Features

I'm sort of in the Emacs-as-an-OS camp, so there are a few goodies in my
Emacs/stumpwm files to check out. It started out with
[Emacs-Live](https://github.com/overtone/emacs-live) but has changed a
bit over time. Here's the gist:

- vim-like keybindings (`.live-packs/evil/config/evil-leader-conf.el`)
- excellent Clojure(Script) support
- basic support for lots of langs
- decent web dev support


The Vim files are pretty old and kind of a mess because I switched to
Emacs a long time ago. Lots of dragons and scary stuff, watch out.


For projects I keep a `~/Gits` folder that categorizes the work so I
know where to find things. Recommended for avoiding headaches:

```
Gits
├── hacks # one-off projects, slightly better than /tmp
├── others # clones of good projects to read through
├── contribs
├── work # things that pay bills
└── personal
    ├── public # stuff on GitHub
    ├── private # stuff umm... not on GitHub
    ├── blog
    └── etc # other dirs for other things
```

## todos

Not going to work on this stuff until I am forced to (like if my
computer crashes again) but:

- improve commands for installing proprietary pkgs like Java and
  google-chrome
- make commands for network installs via push pkgs or pull pkgs
- add rsync calls for syncing private files
- add ibus setup

Send a PR or file an issue if there's something you'd like to tweak.

Happy Hacking!
