;;; --package --- summary
;;; Commentary:
;;; emacs-live.el -- big, bloated file of configs...

;; Author: Ed Babcock <ed@edbabcock.com>

;;; Code:

(live-prepend-packs '(~/.live-packs/clojure-pack))
(live-append-packs '(~/.live-packs/evil))
(live-append-packs '(~/.live-packs/moz))
(live-append-packs '(~/.live-packs/racket)) ;TODO: update repos (add git)
;; (live-append-packs '(~/.live-packs/common-lisp))
(live-append-packs '(~/.live-packs/irc))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; (add-hook 'after-init-hook 'global-company-mode)


(package-initialize)

(defmacro comment
    (&rest stuff)
  nil)


;; change the default font
(comment
 (set-frame-font
  "-unknown-Ubuntu Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1" nil t))

;; make the default scratch language Clojure
(setq initial-major-mode 'clojure-mode)

;; 80 column rule works nicely for a code window + REPL window
(setq-default fill-column 80)

;; persistent emacs sessions
(desktop-save-mode 1)

;; ediprolog
;;(require 'ediprolog)
;;(global-set-key [f10] 'ediprolog-dwim)

;; gap-mode
;;(setq gap-executable "/usr/bin/gap")

;; easier kill buffer key
;; (global-set-key (kbd "C-q") 'kill-buffer)

;; pair braces
(electric-pair-mode 1)

;; Hit C-c <tab> to auto-indent the entire buffer you're in.
(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))
(global-set-key (kbd "C-c <tab>") 'indent-buffer)


;; turn on flycheck except for emacs-lisp
(add-hook 'after-init-hook #'global-flycheck-mode)
(when (equal 'emacs-lisp-mode major-mode)
  (flycheck-mode 0))

;; Always indent after a newline.
(define-key global-map (kbd "RET") 'newline-and-indent)

(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; Keep dired buffers updated when the file system changes.
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; use company mode (TODO: remove old autocomplete)
;; (add-hook 'after-init-hook 'global-company-mode)

;; Emacs editing for term
(evil-set-initial-state 'term-mode 'emacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit Setup

(define-key (current-global-map)
  [remap async-shell-command] 'with-editor-async-shell-command)
(define-key (current-global-map)
  [remap shell-command] 'with-editor-shell-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Hooks

;; Some filetypes that should reload for iterative webpage development
;; (add-hook 'html-mode-hook 'moz-reload-mode)
;; (add-hook 'clojure-mode-hook 'moz-reload-mode)
;; (add-hook 'css-mode-hook 'moz-reload-mode)

;; auto-fill to observe the 80 column rule
;; (add-hook 'clojure-mode-hook 'auto-fill-mode)
;; (add-hook 'scheme-mode-hook 'auto-fill-mode)
;; (add-hook 'emacs-lisp-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
;; Good for math work and keeps out of the way when not doing math
(add-hook 'org-mode-hook (lambda () (set-input-method "TeX")))

;; add nxml-mode for plist + xsd files
(add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))

;; LATEX stuff
;; don't forget C-c C-x \ and C-c C-x C-l
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode


;; HACK: x11 vnc needed this
(setq exec-path (cons "/home/oracle/bin" exec-path))

;; HTML autocomplete
;; (add-hook 'html-mode-hook 'ac-html-enable)

(eval-after-load "sgml-mode"
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))
(tagedit-add-experimental-features)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

(defvar eshell-path-env (getenv "PATH"))
(defvar eshell-rc-script "~/.eshell")


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
;; (setq lisp-indent-function 'scheme-indent-function)

(setq scheme-default-implementation "guile")
(setq quack-default-program "guile")
(setq quack-pretty-lambda-p t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Racket mode

(comment (add-hook 'geiser-repl-mode-hook 'rainbow-delimiters-mode)
         (add-hook 'geiser-repl-mode-hook 'show-paren-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eww browser stuff

(setq url-cookie-confirmation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shen mode

;; TODO: package these hooks in a generic way
(add-hook 'shen-mode-hook 'rainbow-delimiters-mode)
(add-hook 'shen-mode-hook 'show-paren-mode)


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python

;; (setq python-shell-interpreter "/usr/bin/python3")
(setq python-shell-interpreter "/usr/bin/python")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; JavaScript

;; Tern settings
(add-to-list 'load-path "~/.live-packs/js-pack/lib/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

(comment
 (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
 (add-hook 'js2-mode-hook #'js2-refactor-mode))

(comment
 (setq-default
  js2-mode-indent-ignore-first-tab
  js2-strict-inconsistent-return-warning
  js2-global-externs
  '("module" "require" "__dirname" "process" "console" "JSON" "$" "_")))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; set up js2-refactor key bindings
(js2r-add-keybindings-with-prefix "C-c C-m")

(setq ac-js2-evaluate-calls t)

;; JSON settings
(add-to-list 'auto-mode-alist '("\\.json?\\'" . json-mode))

;; indenting for JS + JSON
(setq-default
 js2-basic-offset 2
 js-indent-level 2)


(require 'web-mode)
(require 'flycheck)

;; TODO: try out js2-jsx-mode instead
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mjml?\\'" . web-mode))
;; (add-to-list web-mode-content-types '("jsx" "\\jsx?$"))
;; (add-to-list 'web-mode-comment-formats `("jsx" . "//"))

;; Bind M-n and M-p to navigate to the next/previous errors.
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)

(add-to-list 'flycheck-eslint-rules-directories "/home/tokugawa/.eslintrc")

(setq flycheck-eslint-rules-directories '("/home/tokugawa"))

(setq flycheck-disabled-checkers '(javascript-jshint))
(comment
 (flycheck-add-mode 'javascript-eslint 'js2-mode))

(setq npm-prefix "/usr/local/bin/")

(comment
 (with-eval-after-load "flycheck"
   (flycheck-add-mode 'javascript-eslint 'web-mode)
   (setq flycheck-javascript-eslint-executable (concat npm-prefix "eslint"))))

(comment
 (with-eval-after-load "flycheck"
   (flycheck-add-mode 'javascript-polylint 'web-mode)
   (setq flycheck-javascript-polylint-executable (concat npm-prefix "polylint"))))

;; for React's JSX
;; (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;; (autoload 'jsx-mode "jsx-mode" "JSX mode" t)

;; for gclosure code
;; (flycheck-add-mode 'javascript-gjslint 'js2-mode)

;; TODO: could use defcustom for multiple and give priority to current project

;; (flycheck-add-mode 'html-tidy 'web-mode)
;; (flycheck-add-mode 'css-csslint 'web-mode)

(comment
 (flycheck-define-checker javascript-polylint
   "A linter for Polymer elements

See URL `https://github.com/PolymerLabs/polylint'."
   :command ("polylint" "--stdin")
   :standard-input t
   :error-patterns ((error line-start "  <text>:" line ":" column ":" (message) line-end))
   :modes (web-mode js2-mode javascript-mode)))

;; (flycheck-add-mode 'javascript-polylint 'web-mode)


;; REST client syntax highlighting
;; (add-to-list 'company-backends 'company-restclient)

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

(add-hook 'occur-mode-hook
          (lambda ()
            (evil-add-hjkl-bindings occur-mode-map 'emacs
              (kbd "/")       'evil-search-forward
              (kbd "n")       'evil-search-next
              (kbd "N")       'evil-search-previous
              (kbd "C-d")     'evil-scroll-down
              (kbd "C-u")     'evil-scroll-up
              (kbd "C-w C-w") 'other-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Nix

;; (autoload 'nix-mode "nix-mode" "Major mode for editing Nix expressions." t)
;; (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
;; (add-to-list 'auto-mode-alist '("\\.nix\\.in\\'" . nix-mode))

;; for Nix pkg autocomplete
;; (add-to-list 'company-backends 'company-nixos-options)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; erlang

;; parts of this setup from http://web.archive.org/web/20130718233309/http://bc.tech.coop/blog/070528.html

;; Erlang mode setup
(defun trim (str)
  (replace-regexp-in-string "[\n\s\t]" "" str))

(setq erlang-root-dir "/usr/lib/erlang")
;; handle the version number in the tools-dir name
(let ((tools-dir
       (trim
        (shell-command-to-string "find /usr/lib/erlang/lib -type d -maxdepth 1 -regex '.*/tools-.*'"))))
  (setq load-path (cons (concat tools-dir "/emacs") load-path)))
(setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
(require 'erlang-start)

;; (add-hook 'after-init-hook 'my-after-init-hook)
;; (defun my-after-init-hook ()
;;   (require 'edts-start))
(require 'edts-start)

(define-key erlang-mode-map (kbd "C-M-;") 'erlang-shell)


(comment
 (let ((distel-dir "~/local/distel/elisp"))
   (unless (member distel-dir load-path)
     (setq load-path (append load-path (list distel-dir)))))

 ;; distel setup
 (require 'distel)
 (distel-setup)

 ;; Some Erlang customizations
 (add-hook 'erlang-mode-hook
           (lambda ()
             ;; when starting an Erlang shell in Emacs, default in the node name
             (setq inferior-erlang-machine-options '("-sname" "emacs"))
             ;; add Erlang functions to an imenu menu
             (imenu-add-to-menubar "imenu")))

 ;; A number of the erlang-extended-mode key bindings are useful in the shell too
 (defconst distel-shell-keys
   '(("\C-\M-i"   erl-complete)
     ("\M-?"      erl-complete)
     ("\M-."      erl-find-source-under-point)
     ("\M-,"      erl-find-source-unwind)
     ("\M-*"      erl-find-source-unwind))
   "Additional keys to bind when in Erlang shell.")

 (add-hook 'erlang-shell-mode-hook
           (lambda ()
             ;; add some Distel bindings to the Erlang shell
             (dolist (spec distel-shell-keys)
               (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

 (require 'auto-complete)
 (require 'auto-complete-distel)
 (add-to-list 'ac-sources 'auto-complete-distel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elixir

;; TODO: not working yet...
(eval-after-load 'alchemist-mode
  '(define-key alchemist-mode-map (kdb "C-M-x") 'alchemist-iex-send-region))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-mode

(comment
 (require 'org)
 (define-key global-map "\C-cl" 'org-store-link)
 (define-key global-map "\C-ca" 'org-agenda)
 (setq org-log-done t)

 (setq org-agenda-files '("~/org/general.org"
                          "~/org/startup.org")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commmon LISP

(setq inferior-lisp-program "/usr/bin/sbcl")
