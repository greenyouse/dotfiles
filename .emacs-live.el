(live-prepend-packs '(~/.live-packs/clojure-pack))
(live-append-packs '(~/.live-packs/evil))
(live-append-packs '(~/.live-packs/moz))
(live-append-packs '(~/.live-packs/racket)) ;TODO: update repos (add git)
(live-append-packs '(~/.live-packs/common-lisp))
(live-append-packs '(~/.live-packs/rcirc))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
              '("melpa" . "http://melpa.milkbox.net/packages/") t)


(package-initialize)

;; change the default font
(set-frame-font
 "-unknown-Ubuntu Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" nil t)

;; make the default scratch language Clojure
(setq initial-major-mode 'clojure-mode)

;; 80 column rule works nicely for a code window + REPL window
(setq-default fill-column 80)

;; persistent emacs sessions
(desktop-save-mode 1)

;; ediprolog
(require 'ediprolog)
(global-set-key [f10] 'ediprolog-dwim)

;; gap-mode
(setq gap-executable "/usr/bin/gap")


(defmacro comment
  (&rest stuff)
  nil)

;; easier kill buffer key
(global-set-key (kbd "C-q") 'kill-buffer)


;; turn on flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Hooks

;; Some filetypes that should reload for iterative webpage development
;; (add-hook 'html-mode-hook 'moz-reload-mode)
;; (add-hook 'clojure-mode-hook 'moz-reload-mode)
;; (add-hook 'css-mode-hook 'moz-reload-mode)

;; auto-fill to observe the 80 column rule
; (add-hook 'clojure-mode-hook 'auto-fill-mode)
; (add-hook 'scheme-mode-hook 'auto-fill-mode)
; (add-hook 'emacs-lisp-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
;; Good for math work and keeps out of the way when not doing math
(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))

;; add nxml-mode for plist files
(add-to-list 'auto-mode-alist
             '("\\.plist\\'" . nxml-mode))

;; LATEX stuff
;; don't forget C-c C-x \ and C-c C-x C-l
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode

;; HTML autocomplete
(add-hook 'html-mode-hook 'ac-html-enable)

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
(tagedit-add-experimental-features)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Haskell mode

(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; useful haskell repl code
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Guile Scheme


(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'show-paren-mode)
(add-hook 'scheme-mode-hook
          (function (lambda ()
                      (paren-toggle-matching-quoted-paren 1)
                      (paren-toggle-matching-paired-delimiter 1))))

;; TODO: get parens matching in REPL
(add-hook 'inferior-scheme-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'inferior-scheme-mode-hook 'show-paren-mode)

(custom-set-variables '(scheme-program-name "guile"))
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
(setq auto-mode-alist (cons '("\\.ss\\'" . scheme-mode) auto-mode-alist))

;; smart completions
(autoload 'scheme-smart-complete "scheme-complete" nil t)
(eval-after-load 'scheme
  '(define-key scheme-mode-map "\e\t" 'scheme-smart-complete))

(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
            (eldoc-mode)))
; (setq lisp-indent-function 'scheme-indent-function)

(setq scheme-default-implementation "guile")
(setq quack-default-program "guile")
(setq quack-pretty-lambda-p t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Racket mode

(comment (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
         (add-hook 'geiser-repl-mode-hook 'show-paren-mode)
         (add-hook 'geiser-repl-mode-hook 'electric-pair-mode)) ;TODO: add this to scheme above

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eww browser stuff

(setq url-cookie-confirmation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shen mode

;; TODO: package these hooks in a generic way
(add-hook 'shen-mode-hook 'rainbow-delimiters-mode)
(add-hook 'shen-mode-hook 'show-paren-mode)
(add-hook 'shen-mode-hook 'electric-pair-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Golang

(require 'go-eldoc)
(add-hook 'go-mode-hook 'go-eldoc-setup)

(require 'go-autocomplete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Java

(require 'eclim)
(global-eclim-mode)

(require 'eclimd)

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(custom-set-variables
  '(eclim-eclipse-dirs '("~/local/eclipse"))
  '(eclim-executable "~/local/eclipse/eclim"))

(add-hook 'java-mode-hook 'electric-pair-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

;; (setq python-shell-interpreter "/usr/bin/python3")
(setq python-shell-interpreter "/usr/bin/python")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JavaScript

;; (add-hook 'js2-mode-hook 'ac-js2-mode)
(add-hook 'js2-mode-hook 'electric-pair-mode)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; tell emacs about tern.el
(autoload 'tern-mode "tern.el" nil t)

(setq ac-js2-evaluate-calls t)

(setq auto-mode-alist (cons '("\\.json\\'" . json-mode) auto-mode-alist))

;; indenting for JS + JSON
(setq-default js2-basic-offset 2)
(setq-default js-indent-level 2)

(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


(require 'web-mode)
(require 'flycheck)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; for React's JSX
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; TODO: only activate inside of <script> tags, does web-mode have a hook?
;; (flycheck-add-mode 'javascript-eslint 'web-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Music Player

(require 'emms-player-simple)
(require 'emms-setup)

;; mostly for playing local m3u files
;; (define-emms-simple-player alsaplayer '(file url)
;;   (regexp-opt '(".ogg" ".mp3" ".wav" ".flac" ".pls" ".m3u" "http://"))
;;   "alsaplayer" "--quiet" "--nosave" "--interface=text")

(emms-default-players)
(emms-standard)

;; (add-to-list 'emms-player-list 'emms-player-alsaplayer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Occur Mode helpers

;; from https://masteringemacs.org/article/searching-buffers-occur-mode
(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
   (dolist (buf (buffer-list))
     (with-current-buffer buf
       (if (eq mode major-mode)
           (add-to-list 'buffer-mode-matches buf))))
   buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nix

(autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.nix\\.in\\'" . nix-mode))
