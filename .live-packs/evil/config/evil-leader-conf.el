                                        ; EVIL Leader

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Little macros

;; nice emacs macros that work with evil-leader go here.
(fset 'cljs-comment
      [?I ?\( ?c ?o ?m ?m ?e ?n ?t ?  escape ?, ?l])
(fset 'cljs-remove-comment
      "Hl,kdabxV}=")
(fset 'cljs-println
      [?I ?\( ?p ?r ?i ?n ?t ?l ?n ?  escape ?, ?l])
(fset 'cljs-reader-comment
      [?I ? ?# ?_ ? ? escape])
(fset 'cljs-remove-reader-comment
      [?I escape ?v ?l ?l ?x ?V ?\} ?=])
(fset 'close-shell
      [?, ?x ?\C-m ?y])
(fset 'cljs-comment-divider
   [?i ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; ?\; return ?\; ?\; ?\; ?  escape])


;; buffer navigation macros
(fset 'back-two-buffers
      [?, ?b right return])
(fset 'cider-buffer-jump
      [?, ?b ?c ?i ?d ?e ?r ? return])
(fset 'cider-kill-stacktrace
      [?, ?x ?, ?w ?, ?m ?w])

;; tasker9000 macros
(fset 'tasker9000-next-day
   [?i ?* ?\S-  ?D ?a ?y ?  ?A ?f ?t ?e ?r return tab ?1 ?. ?  ?P ?r ?i ?m ?a ?r ?y ?: return ?2 ?. ?S ?e ?o ?n ?c ?o ?n ?d ?a ?r ?y ?: return ?3 ?. ?T ?e ?r ?t ?i ?a ?r ?y ?: return return ?  ?  ?- ?  ?B ?r ?e ?a ?k ?s ?: return ?  ?  ?- ?  ?D ?o ?w ?n ?t ?i ?m ?e ?: return escape])
;; FIXME: broken
(fset 'tasker9000-cycle-day
   [?V ?\} ?\{ ?\} ?\{ ?\] ?\] ?d ?w ?d ?w ?A ?T ?o ?d ?a ?y escape ?\] ?\] ?j ?w ?D ?A ?T ?o ?m ?o ?r ?r ?o ?w escape ?G ?o escape ?I ?* ?\S-  ?D ?a ?y ?  ?A ?f ?t ?e ?r ?  ?T ?o ?m ?o ?r ?r ?o ?w return tab ?1 ?. return tab backspace backspace backspace backspace backspace ?  ?P ?r ?i ?m ?a ?r ?y ?: ?  return tab backspace backspace backspace ?2 ?. ?  ?S ?e ?c ?o ?n ?d ?a ?r ?y ?: ?  return tab backspace backspace backspace ?3 ?. ?  ?T ?e ?r ?t ?i ?a ?r ?y ?: ?  escape])


;; word edtiing macros
(fset 'delete-up-one
      [?H ?d ?w ?i backspace ?  escape ?V ?=])
(fset 'delete-down-one
      [?i return escape ?j ?H ?d ?w ?i backspace ?  escape ?H ?W ?V ?=])

;; magit init macro (use before calling magit-status)
(fset 'split-and-jump-window
   [?, ?m ?s ?, ?w])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Various fns

(defun lein-project-root (&optional file)
  "Finds the leiningen project root"
   (locate-dominating-file (or file default-directory) "project.clj"))

;;(lambda () (interactive) (cider-cljs-reload)) ; reload files automagically for cider
;; Removed because it's no longer really necessary with modern cljs
(defun cider-cljs-reload ()
  "Reloads the Clojure(Script) files inside src.
   When doing a bREPL, scrub the repl state with moz-repl (,mr) and run this
   to reset the REPL state (kinda similar to tools.namespace?). Add more folders
   if needed."
  (let ((src-files (shell-command-to-string (concat "find " (expand-file-name "src" (lein-project-root)) " -type f | grep \"**.clj\""))))
    (mapcar (lambda (some-file)
              (cider-load-file some-file))
            (split-string (concat src-files)))) (message "Files loaded, hack away!"))

(defun cljs-lookup()
  "Lookup the item under the cursor on ClojureDocs"
  (interactive)
  (let (myWord myUrl)
    (setq myWord
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'symbol)))
    (setq myWord (replace-regexp-in-string " " "_" myWord))
    (setq myUrl (concat "http://clojuredocs.org/search?x=0&y=0&q=" myWord))
    (browse-url myUrl)))

(defun cider-figwheel-repl ()
  (interactive)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

;; some custom emacs functions should eventually use this
(defun my-macro-query ()
  "Prompt for input using minibuffer during kbd macro execution.
    With prefix argument, allows you to select what prompt string to use.
    If the input is non-empty, it is inserted at point."
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                  (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

;; adapted from SO
;; http://stackoverflow.com/questions/13981899/how-can-i-kill-all-buffers-in-my-emacs
(defun nuke-all-buffers ()
  (interactive)
  (let ((answer (yes-or-no-p "Initiate nuclear countdown sequence?")))
    (when answer
      ;; for last minute stopping and more silliness
      (sleep-for 1)
      (message "3")
      (sleep-for 1)
      (message "2")
      (sleep-for 1)
      (message "1")
      (sleep-for 1 300) ; regrets?
      (mapc 'kill-buffer (buffer-list))
      (delete-other-windows))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymap

(setq evil-leader/leader ",")


(evil-leader/set-key

  ;; General stuff
  "f" 'find-file
  "b" 'switch-to-buffer
  "x" 'kill-buffer
  "s" 'evil-buffer
  "d" 'back-two-buffers
  "w" (kbd "C-x o")
  "l" 'live-paredit-forward-slurp-sexp-neatly  ; paredit settings
  "k" 'paredit-forward-barf-sexp
  "j" 'paredit-backward-barf-sexp
  "h" 'paredit-backward-slurp-sexp
  "[" 'buf-move-left
  "]" 'buf-move-up

  ;; Undo-tree
  "uu" 'undo-tree-visualize

  ;; various Clojure(Script) + Cider commands
  "mw" 'delete-other-windows
  "ms" 'split-window-right
  "mv" 'split-window-vertically
  "mn" 'cider-jack-in
  "mq" 'cider-quit
  "mQ" 'cider-quit
  "ma" (kbd "C-M-x") ; eval-form
  "md" 'cider-doc
  "mb" 'cider-buffer-jump
  "mm" (lambda () (interactive) (scratch))
  "me"  'magit-status
  "mcc" (kbd "C-c M-o") ; clear REPL
  "mf" (kbd "C-c C-l") ; eval-file
  "mcn" (kbd "C-c M-n") ; switch to current namespace
  "mcs" (kbd "C-c C-s") ; show source
  "mzz" (kbd "A (cemerick.piggieback/cljs-repl)") ; launch cljs piggieback repl (no js interop)
                                        ; \ escapes followed by i allow for spaces in macro text
  "mzb" (kbd "A (require\ i'cljs.repl.browser) (cemerick.piggieback/cljs-repl\ i:repl-env (cljs.repl.browser/repl-env\ i:port\ i9000))") ; cljs brepl (preferred repl method)
  "mzp" (kbd "A  (cemerick.piggieback/cljs-repl\ i:repl-env\ i(cemerick.austin/exec-env))") ;; phantomjs brepl (won't work with native, non closure js libs)
  "mzw" (kbd "A (require \ i'weasel.repl.websocket) (cemerick.piggieback/cljs-repl\ i:repl-env (weasel.repl.websocket/repl-env\
 i:ip\ i\"0.0.0.0\"\ i:port\ i9001)") ; weasel repl
  "mzf" 'cider-figwheel-repl
  "mt" (kbd "A (run-tests)") ; for cljs testing, reload with browser refresh for now
  "ml" 'linum-mode
  "mp" (kbd "C-x p") ; refresh browser ala moz-repl
  "mr" 'moz-reload-mode
  "mx" 'lcljs-start-hacking
  "mk" 'lcljs-stop-hacking
  "mo" (lambda () (interactive) (browse-url "http://localhost:8000")) ; Open  a bREPL for cljs hacking

  ;; cljs macros
  "cc" 'cljs-comment
  "cd" 'cljs-remove-comment
  "cp" 'cljs-println
  "cr" 'cljs-reader-comment
  "ce" 'cljs-remove-reader-comment
  "cu" 'cljs-lookup
  "cl" 'cljs-comment-divider

  ;; clj namespace sorting
  "cs" 'sort-lines

  ;; emacs helpers
  "," 'smex
  "ee" 'smex
  "es" (lambda () (interactive) (ansi-term "bash"))
  "ed" 'close-shell
  "eff" 'helm-imenu
  "efm" 'helm-imenu-in-all-buffers

  ;; general word-editing macros
  "gk" 'delete-up-one
  "gj" 'delete-down-one
  "gc" 'ispell
  "gs" 'ispell-word
  "gw" 'wc

  ;; tasker9000 macros
  "tn" 'tasker9000-next-day
  "tc" 'tasker9000-cycle-day

  ;; super dangerous comand of death only to be used in serious, ok whatever...
  ";xxx" 'nuke-all-buffers)
