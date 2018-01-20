                       ; EVIL

;; jump to end/beginning of line
(define-key evil-normal-state-map "H" 'evil-beginning-of-line)
(define-key evil-normal-state-map "L" 'evil-end-of-line)

;; swap s for jump item
(define-key evil-normal-state-map "s" 'evil-jump-item)
(define-key evil-normal-state-map "%" 'evil-substitute)

;; better editing boundaries (doesn't group multiple text inserts together)
(setq evil-want-fine-undo t)

;; jj for ending edit
(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)