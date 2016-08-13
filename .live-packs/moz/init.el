(live-add-pack-lib "moz")


(require 'moz)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

(global-set-key (kbd "C-x p")
                (lambda ()
                  (interactive)
                  (comint-send-string (inferior-moz-process)
                                      "setTimeout(function(){content.document.location.reload(true);}, '500');")))

(require 'moz-reload)
(autoload 'moz-reload-mode "moz-reload-mode" "Reload mode for Moz REPL" t)
