;; Racket Pack

;(live-add-pack-lib "geiser")
(live-add-pack-lib "quack")
;(live-add-pack-lib "ac-geiser")

(require 'quack)
;(require 'geiser)
;(require 'ac-geiser)

(comment (add-hook 'geiser-mode-hook 'ac-geiser-setup)
         (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
         (eval-after-load "auto-complete"
           '(add-to-list 'ac-modes 'geiser-repl-mode))

         (setq geiser-active-implementations '(racket)))
