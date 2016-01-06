(when macosp
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper))

(global-unset-key (kbd "C-x C-c"))
;;(global-set-key (kbd "C-x C-c") 'delete-frame)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

(global-unset-key (kbd "C-x m")) ; disable sendmail

(define-key minibuffer-local-map [up]     'previous-history-element)
(define-key minibuffer-local-map [down]   'next-history-element)
(define-key minibuffer-local-map [C-up]   'previous-complete-history-element)
(define-key minibuffer-local-map [C-down] 'next-complete-history-element)


(global-set-key (kbd "s-(") "{")
(global-set-key (kbd "s-)") "}")
(global-set-key (kbd "s-/") "\\")
(global-set-key (kbd "s-7") "|")
(global-set-key (kbd "s-8") "[")
(global-set-key (kbd "s-9") "]")

(global-set-key (kbd "C-s-j") 'join-line-or-lines-in-region)
(global-set-key (kbd "s-J")   (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "s-j")   'delete-indentation)

(global-set-key (kbd "s-S") 'sort-fields)
(global-set-key (kbd "s-s") 'sort-lines)

(global-set-key (kbd "C-c g") 'google)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c |") 'align-regexp)

(global-unset-key (kbd "C-c e"))
(global-set-key (kbd "C-c e s") 'give-me-a-scratch-buffer-now)

(global-unset-key (kbd "C-c i"))
(global-set-key (kbd "C-c i -")  'insert-separator)
(global-set-key (kbd "C-c i T")  'insert-timestamp)
(global-set-key (kbd "C-c i c")  'insert-date-and-time)
(global-set-key (kbd "C-c i d")  'insert-date)
(global-set-key (kbd "C-c i m")  'insert-kbd-macro)
(global-set-key (kbd "C-c i o")  'insert-org-header)
(global-set-key (kbd "C-c i p")  'insert-path)
(global-set-key (kbd "C-c i s")  'insert-change-signature)
(global-set-key (kbd "C-c i t")  'insert-time)
(global-set-key (kbd "C-c i w")  'insert-week-number)

(global-unset-key (kbd "C-c t"))
(global-set-key (kbd "C-c t c")  'transform-region-to-lower-camel-case)
(global-set-key (kbd "C-c t C")  'transform-region-to-upper-camel-case)
(global-set-key (kbd "C-c t s")  'transform-region-to-snake-case)
(global-set-key (kbd "C-c t -")  'transform-region-to-dashed)

(global-unset-key (kbd "C-c o"))
(global-set-key (kbd "C-c o o") 'find-file-at-point)
(global-set-key (kbd "C-c o p") 'find-file-in-project)
(global-set-key (kbd "C-c o el") (ffip-create-pattern-file-finder "*.el"))
(global-set-key (kbd "C-c o ja") (ffip-create-pattern-file-finder "*.java"))
(global-set-key (kbd "C-c o js") (ffip-create-pattern-file-finder "*.js"))

(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-q") 'read-only-mode)
(global-set-key (kbd "C-x C-r") 'revert-buffer)
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)

(global-set-key (kbd "M-C")  'comment-region)
(global-set-key (kbd "M-F")  'auto-fill-mode)
(global-set-key (kbd "M-I")  'indent-region)
(global-set-key (kbd "M-T")  'toggle-truncate-lines)
(global-set-key (kbd "M-U")  'uncomment-region)
(global-set-key (kbd "M-o")  'duplicate-line)

(global-set-key (kbd "C-0")  'delete-window)
(global-set-key (kbd "C-1")  'delete-other-windows)
(global-set-key (kbd "C-2")  'split-window-below)
(global-set-key (kbd "C-3")  'split-window-right)
(global-set-key (kbd "C-7")  'comment-or-uncomment-current-line-or-region)

(global-set-key (kbd "C-c 0") (lambda ()
                                (interactive)
                                (find-file
                                 (expand-file-name "setup.el"
                                                   user-emacs-directory))))

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "s-å")  'mc/mark-previous-like-this)
(global-set-key (kbd "s-ä")  'mc/mark-next-like-this)
(global-set-key (kbd "s-ö")  'mc/mark-all-like-this-dwim)
(global-set-key (kbd "s-Ä")  'mc/mark-more-like-this-extended)

(global-set-key (kbd "<C-S-up>")   'move-text-up)
(global-set-key (kbd "<C-S-down>") 'move-text-down)

(global-set-key [C-M-up]    'backward-paragraph)
(global-set-key [C-M-down]  'forward-paragraph)

(global-set-key [C-return]  'newline-and-indent)

(unless macosp
  (global-set-key [M-delete]  'kill-word))

(global-set-key [insert]    'overwrite-mode)
(global-set-key [S-insert]  'insert-separator)
(global-set-key [C-insert]  'insert-separator-dashed)
(global-set-key [M-insert]  'insert-separator-text)

(global-set-key [home]    'beginning-of-line)
(global-set-key [S-home]  'cursor-to-top-of-window)
(global-set-key [C-home]  'beginning-of-buffer)
(global-set-key [M-home]  'this-line-to-top-of-window)

(global-set-key [end]      'end-of-line)
(global-set-key [S-end]    'cursor-to-bottom-of-window)
(global-set-key [C-end]    'end-of-buffer)
(global-set-key [M-end]    'this-line-to-bottom-of-window)
(global-set-key [C-M-end]  'goto-line)

(global-set-key [C-M-prior]  'shift-region-left)
(global-set-key [C-M-next]   'shift-region-right)

(global-set-key [f8]    'next-error)
(global-set-key [M-f8]  'previous-error)

(global-set-key [f9]    'compile)
(global-set-key [M-f9]  'recompile)

(global-set-key (kbd "<f10>") 'vc-examine)
(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

(global-set-key [f11]    'call-last-kbd-macro)
(global-set-key [M-f11]  'apply-macro-to-region-lines)
(global-set-key [S-f11]  'name-last-kbd-macro)
(global-set-key [C-f11]  'insert-kbd-macro)

(global-set-key [kp-separator] [?.])

(global-set-key [f12]    'buffer-switch)
(global-set-key [M-f12]  (lambda () (interactive) (buffer-switch 3)))
(global-set-key [S-f12]  (lambda () (interactive) (buffer-switch 4)))
(global-set-key [C-f12]  'bury-buffer)
(global-set-key [s-f12]  'rotate-windows)

(global-set-key (kbd "C-z") 'undo)

(provide 'key-bindings)
