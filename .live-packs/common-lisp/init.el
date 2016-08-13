;; SLIME Pack

(live-add-pack-lib "slime")
(add-to-list 'load-path "~/.live-packs/slime/lib/slime/")
(require 'slime-autoloads)

;;(setq inferior-lisp-program "/usr/local/bin/lisp")
(setq inferior-lisp-program "/usr/bin/clisp")
(setq slime-contribs '(slime-fancy))
