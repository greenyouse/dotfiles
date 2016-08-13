(define-minor-mode moz-reload-mode
  "Moz Reload Minor Mode"
  nil " Reload" nil
  (if moz-reload-mode
      ;; Edit hook buffer-locally.
      (add-hook 'after-save-hook 'moz-reload nil t)
    (remove-hook 'after-save-hook 'moz-reload t)))

(defun moz-reload ()
    ;; wait 2 seconds for cljsbuild to compile
  (run-with-idle-timer 2 nil 'moz-firefox-reload))

(defun moz-firefox-reload ()
  (comint-send-string (inferior-moz-process)
                      "setTimeout(function(){content.document.location.reload(true);}, '500');"))

(provide 'moz-reload)
