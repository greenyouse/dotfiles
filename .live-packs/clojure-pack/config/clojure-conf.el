(require 'clojure-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fancy font locking

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(require 'clojure-mode-extra-font-locking)

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation

;; indentation changed, rebinding keys
(add-hook 'clojure-mode-hook '(lambda ()
                                (local-set-key (kbd "RET") 'newline-and-indent)))

;; (setq clojure-align-forms-automatically 't)

;; align args
;; (setq clojure-indent-style :always-align)

;; doing clj indentation
;; (put 'implement 'clojure-backtracking-indent '(4 (2)))
;; (put 'letfn 'clojure-backtracking-indent '((2) 2))
;; (put 'proxy 'clojure-backtracking-indent '(4 4 (2)))
;; (put 'reify 'clojure-backtracking-indent '((2)))
;; (put 'deftype 'clojure-backtracking-indent '(4 4 (2)))
;; (put 'defrecord 'clojure-backtracking-indent '(4 4 (2)))
;; (put 'defprotocol 'clojure-backtracking-indent '(4 (2)))
;; (put 'extend-type 'clojure-backtracking-indent '(4 (2)))
;; (put 'extend-protocol 'clojure-backtracking-indent '(4 (2)))
;; (put 'specify 'clojure-backtracking-indent '(4 (2)))
;; (put 'specify! 'clojure-backtracking-indent '(4 (2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Clojure stuff


;;Treat hyphens as a word character when transposing words
(defvar clojure-mode-with-hyphens-as-word-sep-syntax-table
  (let ((st (make-syntax-table clojure-mode-syntax-table)))
    (modify-syntax-entry ?- "w" st)
    st))

(defun live-transpose-words-with-hyphens (arg)
  "Treat hyphens as a word character when transposing words"
  (interactive "*p")
  (with-syntax-table clojure-mode-with-hyphens-as-word-sep-syntax-table
    (transpose-words arg)))

(define-key clojure-mode-map (kbd "M-t") 'live-transpose-words-with-hyphens)


(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq buffer-save-without-query t)))

;; use clojurescript mode
(comment
 (setq auto-mode-alist (append '(("\\.cljs$" . clojure-mode))
                               auto-mode-alist)))

(setq auto-mode-alist (append '(("\\.cljc$" . clojure-mode))
                        auto-mode-alist))

(dolist (x '(scheme emacs-lisp lisp clojure))
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'enable-paredit-mode)
  (add-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(defun live-warn-when-cider-not-connected ()
      (interactive)
      (message "nREPL server not connected. Run M-x cider or M-x cider-jack-in to connect."))

(define-key clojure-mode-map (kbd "C-M-x")   'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-x C-e") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-e") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-l") 'live-warn-when-cider-not-connected)
(define-key clojure-mode-map (kbd "C-c C-r") 'live-warn-when-cider-not-connected)
