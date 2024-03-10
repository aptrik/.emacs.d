;; defaults.el -*- lexical-binding: t -*-

(set-language-environment "UTF-8")
(setq default-input-method nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq
 blink-cursor-blinks                    0
 focus-follows-mouse                    t
 interprogram-cut-function              'gui-select-text
 interprogram-paste-function            'gui-selection-value
 mouse-autoselect-window                nil
 mouse-drag-copy-region                 t
 mouse-yank-at-point                    t
 read-file-name-completion-ignore-case  nil
 save-interprogram-paste-before-kill    t
 select-active-regions                  nil
 suggest-key-bindings                   nil)

(setq
 select-enable-clipboard t
 select-enable-primary t)

(setq-default
 auto-save-default             nil
 backup-inhibited              t
 buffers-menu-max-size         nil
 completion-cycle-threshold    nil
 ;;confirm-kill-emacs            'y-or-n-p
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
 truncate-lines                t
 version-control               nil
 visible-bell                  t
 x-stretch-cursor              t)

(setq
 default-major-mode              'indented-text-mode
 delete-by-moving-to-trash       nil ; Disable trash can support.
 eval-expression-print-level     nil
 frame-inhibit-implied-resize    t
 frame-resize-pixelwise          t
 garbage-collection-messages     nil
 history-delete-duplicates       t
 history-length                  t
 isearch-allow-scroll            t
 kill-ring-max                   200
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
 truncate-partial-width-windows  nil
 window-resize-pixelwise         t
 yank-excluded-properties        t) ; do not paste any properties

(when (display-graphic-p)
 (setq frame-title-format '("" user-login-name "@" system-name)))

;;; Turn on some global minor modes.
(auto-compression-mode 1)
(blink-cursor-mode 1)
(column-number-mode 1)

(setq cua-enable-cua-keys   nil ; only for rectangles
      cua-delete-selection  nil)
(cua-mode 1)


(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)


;;; File hooks.
(if (fboundp 'find-file--follow-symlink)
    (add-hook 'find-file-hook 'find-file--follow-symlink))
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

;;; defaults.el ends here
