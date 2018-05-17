# dotfiles

Here are some configs that I've been using for a while.

My primary setup is Ubuntu but this should work for OSX too.

To get rolling take a look at the
[woot](https://github.com/greenyouse/dotfiles/blob/master/woot)
script which handles installs and backups. There are installs for
Ubuntu, Lubuntu, and OSX.

## Install

Download and unzip the repo:
https://github.com/greenyouse/dotfiles/archive/master.zip

The install should be something like:

```sh
mv dotfiles ~/.config
cd ~/.config/dotfiles
# assumes your $HOME is clear of dotfiles like .bashrc or .profile
./woot install client
```

Backups go like this:

```sh
cd ~/.config/dotfiles
./woot backup storage-drive/backup-folder
```

Uninstall:
```sh
./woot uninstall
```


## Features

I'm sort of in the Emacs-as-an-OS camp, so there are a few goodies in my
Emacs/stumpwm files to check out. It started out with
[Emacs-Live](https://github.com/overtone/emacs-live) but has changed a
bit over time. Here's the gist:

- vim-like keybindings ([evil-leader-conf.el](https://github.com/greenyouse/dotfiles/blob/master/.live-packs/evil/config/evil-leader-conf.el))
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
└── personal
    ├── public # stuff on GitHub
    ├── private # stuff not on GitHub
    ├── blog
    └── etc # other dirs for other things
```

Happy Hacking!
