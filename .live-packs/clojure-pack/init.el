;; (live-add-pack-lib "clojure-mode")
;; (live-add-pack-lib "cider")
;; auto-complete
(live-add-pack-lib "fuzzy-el")
(live-add-pack-lib "popup-el")
(live-add-pack-lib "auto-complete")
;; highlighting/flashing
(live-add-pack-lib "eval-sexp-fu")
;; mic-paren

;; other various libs
(live-add-pack-lib "rainbow-delimiters")
(live-add-pack-lib "uuid")
(live-add-pack-lib "simple-httpd")
(live-add-pack-lib "cljsbuild-mode")
(live-add-pack-lib "live-cljs-mode")

;; make sure we require everything
(require 'ac-cider)
(require 'auto-complete)
(require 'auto-complete-config)
(require 'cider)
(require 'clojure-mode)
;; (require 'clojure-test-mode)
(require 'fuzzy)
(require 'popup)
(require 'mic-paren)
(require 'simple-httpd)
(require 'cljsbuild-mode)
(require 'live-cljs-mode)
(require 'rainbow-delimiters)
(require 'paredit)

;; load the configs
(live-load-config-file "mic-paren-conf.el")
(live-load-config-file "paredit-conf.el")
(live-load-config-file "auto-complete-conf.el")
(live-load-config-file "clojure-conf.el")
(live-load-config-file "cider-conf.el")

;; TODO: pull all the submodules so each lib gets updated
