;;; rcirc pack

;; TODO: may need to add
;(live-add-pack-lib "rcirc-color")
(eval-after-load 'rcirc '(require 'rcirc-color))


(live-load-config-file "general.el")
(live-load-config-file "notify.el")
