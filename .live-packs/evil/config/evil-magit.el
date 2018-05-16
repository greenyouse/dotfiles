(require 'magit)

(setq evil-magit-state 'motion)

;; jump back to emacs mode for emacs things
(evil-set-initial-state #'magit-popup-mode 'emacs)
(evil-set-initial-state #'magit-mode 'emacs)
(evil-set-initial-state #'magit-blame 'emacs)