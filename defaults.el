(defalias 'yes-or-no-p 'y-or-n-p)

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(when macosp
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash/emacs")
  (setq ns-pop-up-frames nil))

(setq
 focus-follows-mouse                  t
 interprogram-cut-function            'x-select-text
 interprogram-paste-function          'x-selection-value
 mouse-autoselect-window              t
 mouse-drag-copy-region               t
 mouse-yank-at-point                  t
 save-interprogram-paste-before-kill  t
 select-active-regions                nil
 x-select-enable-clipboard            t
 x-select-enable-primary              t)

(setq-default
 auto-save-default             nil
 backup-inhibited              t
 buffers-menu-max-size         nil
 completion-cycle-threshold    nil
 confirm-kill-emacs            'y-or-n-p
 delete-active-region          nil
 echo-keystrokes               0.1
 fill-column                   72
 indent-tabs-mode              nil
 indicate-buffer-boundaries    'left
 indicate-empty-lines          t
 make-backup-files             nil
 mark-even-if-inactive         t
 require-final-newline         nil
 standard-indent               4
 tab-width                     8
 trim-versions-without-asking  t
 truncate-lines                nil
 version-control               nil
 visible-bell                  t
 x-stretch-cursor              t)

(setq
 default-major-mode              'indented-text-mode
 delete-by-moving-to-trash       nil ; Disable trash can support.
 eval-expression-print-level     nil
 frame-resize-pixelwise          t
 garbage-collection-messages     nil
 history-delete-duplicates       t
 history-length                  t
 large-file-warning-threshold    100000000
 line-move-visual                nil ; Make Emacs 23 move like old emacs
 make-pointer-invisible          t
 mouse-yank-at-point             t
 next-line-add-newlines          nil ; Don't add newlines with 'down' key.
 print-escape-newlines           t
 save-abbrevs                    nil
 scroll-conservatively           0
 scroll-preserve-screen-position 'always
 scroll-step                     0
 sentence-end                    "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*"
 sentence-end-double-space       nil ; make M-[ae] work
 tab-always-indent               'complete
 truncate-partial-width-windows  nil
 yank-excluded-properties        t) ; do not paste any properties

(xlaunch
 (setq frame-title-format
       '((:eval (if (and (buffer-file-name) (file-regular-p (buffer-file-name)))
                    "%*"
                  ""))
         (:eval (or (buffer-name) "%b"))
         " <" user-login-name "%@" (:eval (system-name)) ">")))


;;; Turn on some minor modes.
(auto-compression-mode 1)
(blink-cursor-mode -1)
(column-number-mode 1)
(delete-selection-mode 1)
(electric-indent-mode -1)
(global-font-lock-mode 1)
(line-number-mode 1)
(transient-mark-mode 1)
(which-func-mode 1)
(winner-mode 1)

(setq cua-enable-cua-keys   nil ; only for rectangles
      cua-delete-selection  nil)
(cua-mode 1)


(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)


;;; File hooks.
(add-hook 'find-file-hook 'find-file--follow-symlink)
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)


;;; Modify `completion-ignored-extensions'.
(setq completion-ignored-extensions
      (remove ".pdf" completion-ignored-extensions))
(setq completion-ignored-extensions
      (remove ".bin" completion-ignored-extensions))
(setq completion-ignored-extensions
      (remove ".lib" completion-ignored-extensions))
(setq completion-ignored-extensions
      (remove ".log" completion-ignored-extensions))
(add-to-list 'completion-ignored-extensions ".ali")
(add-to-list 'completion-ignored-extensions ".adt")
(add-to-list 'completion-ignored-extensions ".class")
(add-to-list 'completion-ignored-extensions ".rbc")


;; Re-enable commands.
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'defaults)
