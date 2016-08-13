evil-pack
==============

A pack for easier Clojure(Script) hacking with [Evil](http://www.emacswiki.org/emacs/Evil)
and [Evil Leader](https://github.com/cofi/evil-leader). This has lots
of new keybinds for emacs that really change how emacs works. If you're
coming from Vim and or are getting tired of emacs' `C-*` `M-*`, this
might be useful.


# Install

First get emacs-live, then add this to `.emacs-live.el`:
```elisp
(live-append-packs '(~/.live-packs/evil))
```

# Customizing

I added my own settings to `config/evil-leader.el` but you'll probably
want to customize it for yourself over time. Send me a pull requests
if you have keybinds you'd like to contribute.

The leader key for this pack is: <kbd>,</kbd>


### General

Keybind        | Description
---------------|------------------------------------
<kbd>f</kbd>   | Find a file
<kbd>b</kbd>   | Switch to buffer
<kbd>x</kbd>   | Kill buffer
<kbd>s</kbd>   | Jump to last buffer
<kbd>d</kbd>   | Jump to second to last buffer
<kbd>w</kbd>   | Switch window
<kbd>mw</kbd>  | Delete other windows
<kbd>ee</kbd>  | Emacs M-x
<kbd>es</kbd>  | Emacs shell
<kbd>ed</kbd>  | Close an emacs shell

### Paredit

Keybind        | Description
---------------|------------------------------------
<kbd>l</kbd>   | Slurp forward
<kbd>k</kbd>   | Barf forward
<kbd>j</kbd>   | Barf backwards
<kbd>h</kbd>   | Slurp backwards

### nrepl

Keybind        | Description
---------------|-------------------------------------------------
<kbd>mn</kbd>  | nrepl jack in
<kbd>mq</kbd>  | Quit nrepl
<kbd>ma</kbd>  | Eval an s-exp
<kbd>mb</kbd>  | Jump to the nrepl buffer
<kbd>mcc</kbd> | Clear the nrepl buffer
<kbd>mf</kbd>  | Eval the current buffer in nrepl
<kbd>mcn</kbd> | Switch current namespace
<kbd>mcs</kbd> | Show source code of item under cursor
<kbd>mzz</kbd> | Launch cljs piggieback (no js interop)
<kbd>mzb</kbd> | cljs bREPL (preferred repl method)
<kbd>mzp</kbd> | Phantom bREPL (no non-closure js libs)
<kbd>mt</kbd>  | For [clojurescript.test](https://github.com/cemerick/clojurescript.test) testing
<kbd>ml</kbd>  | Reload files in current project for nrepl
<kbd>mr</kbd>  | Refresh browser page \w [Moz-REPL](https://addons.mozilla.org/en-US/firefox/addon/mozrepl/) + [moz-pack](https://github.com/greenyouse/moz-repl-pack)

### org-mode

Keybind        | Description
---------------|-------------------------------------------------
<kbd>o1</kbd>  | Toggle sublevel 1
<kbd>o2</kbd>  | Toggle sublevel 2
<kbd>o3</kbd>  | Toggle sublevel 3
<kbd>o4</kbd>  | Toggle sublevel 4
<kbd>o5</kbd>  | Toggle sublevel 5
<kbd>o6</kbd>  | Toggle children 1
<kbd>o7</kbd>  | Toggle children 2
<kbd>o8</kbd>  | Toggle children 3
<kbd>o9</kbd>  | Toggle children 4
<kbd>o0</kbd>  | Toggle children 5
<kbd>oo</kbd>  | Toggle org levels (nomally TAB)
<kbd>os</kbd>  | Sort items
<kbd>ol</kbd>  | Add a level
<kbd>oh</kbd>  | Move up one item
<kbd>oj</kbd>  | Move down one item
<kbd>ok</kbd>  | Delete a level
<kbd>o/</kbd>  | Search in org mode

### ClojureScript

Keybind        | Description
---------------|-------------------------------------------------
<kbd>cc</kbd>  | Comment with ClojureScript ``` (comment ```
<kbd>cd</kbd>  | Delete a ClojureScript ``` (comment ```
<kbd>cp</kbd>  | Create a ``` (println ```
<kbd>cr</kbd>  | ClojureScript reader comment ``` #_ ```
<kbd>ce</kbd>  | Remove a reader comment
<kbd>cu</kbd>  | Lookup item on [clojuredocs.org](http://clojuredocs.org/)


