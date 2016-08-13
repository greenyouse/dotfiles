(live-add-pack-lib "evil")
(live-add-pack-lib "evil-leader")
(live-add-pack-lib "emacs-powerline")

(require 'evil-leader)
(setq evil-leader/in-all-states t)
(evil-mode nil)
(global-evil-leader-mode 1)

(require 'powerline)

(require 'evil)
(evil-mode 1)
(require 'powerline)

(live-load-config-file "evil-leader-conf.el")
(live-load-config-file "evil-conf.el")
(live-load-config-file "emacs-powerline-conf.el")
