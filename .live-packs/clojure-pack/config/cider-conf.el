(require 'cider)
(require 'ac-cider)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cider hooks

(comment
 (add-hook 'nrepl-connected-hook 'live-nrepl-set-print-length))

;; cider repl with ac-cider for autocompletion
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-setup) ;TODO: move these into lambdas?
(add-hook 'cider-repl-mode-hook 'show-paren-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'cider-mode)
     (add-to-list 'ac-modes 'cider-repl-mode)))
(comment (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))
(add-hook 'cider-repl-mode-hook
          (lambda ()
             (paredit-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cider Settings

(setq cider-popup-stacktraces t)
(setq cider-popup-stacktraces-in-repl t)
(add-to-list 'same-window-buffer-names "*cider*")

;; prevent cider buffer from popping up when started
(setq cider-repl-pop-to-buffer-on-connect nil)
(setq cider-show-error-buffer 'except-in-repl)
(setq nrepl-hide-special-buffers t)


(setq cider-lein-parameters "repl :headless :host localhost")