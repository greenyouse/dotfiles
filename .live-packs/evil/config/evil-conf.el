                       ; EVIL

;; jump to end/beginning of line
(define-key evil-normal-state-map "H" 'evil-beginning-of-line)
(define-key evil-normal-state-map "L" 'evil-end-of-line)

;; swap s for jump item
(define-key evil-normal-state-map "s" 'evil-jump-item)
(define-key evil-normal-state-map "%" 'evil-substitute)
