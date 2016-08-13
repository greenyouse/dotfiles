;;; Notification Settings for rcirc

(defun my-rcirc-print-hook (process sender response target text)
  "Sound notification for when my handle gets called in an IRC channel"
  (when (and (string-match (regexp-quote (rcirc-nick process)) text)
	     (not (string= (rcirc-nick process) sender))
	     (not (string= (rcirc-server-name process) sender)))
    (call-process "espeak" nil nil nil "name ping, check irc")))

(add-hook 'rcirc-print-functions 'my-rcirc-print-hook)


;;; minor mode for general message pings in a channel (highyly advanced config)
(define-minor-mode rcirc-ding
  "Make some noise whenever there is activity in the current buffer."
  nil " Ding")


(setq rcirc-ding-this
      (lambda (&rest ignore)
	(call-process "espeak" nil nil nil "beep beep")))


(defun rcirc-ding-maybe (&rest ignore)
  "When minor-mode `rcirc-ding' is active, `rcirc-ding-this' is called.
   See `rcirc-activity-hooks' for more."
  (when rcirc-ding
    (run-with-idle-timer
     3 nil rcirc-ding-this)))

(provide 'rcirc-ding)

(add-hook 'rcirc-activity-hooks 'rcirc-ding-maybe)
