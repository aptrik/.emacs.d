;;-----------------------------------------------------------------------------
;;; archive

(add-hook 'archive-mode-hook 'setup--truncate-lines)
(add-hook 'tar-mode-hook     'setup--truncate-lines)

;;-----------------------------------------------------------------------------
;;; bookmarks

(require 'bookmark+)

(setq bookmark-save-flag 1
      bmkp-last-as-first-bookmark-file nil)

;;-----------------------------------------------------------------------------
;;; browse-kill-ring

(setq browse-kill-ring-quit-action 'save-and-restore)

;;-----------------------------------------------------------------------------
;;; bs

(setq bs-default-configuration       "all"
      bs-alternative-configuration   "files-and-scratch"
      bs-maximal-buffer-name-column  60
      bs-minimal-buffer-name-column  45
      bs-attributes-list '(("" 1 1 left bs--get-marked-string)
                           ("M" 1 1 left bs--get-modified-string)
                           ("R" 2 2 left bs--get-readonly-string)
                           ("Buffer" bs--get-name-length 10 left bs--get-name)
                           ("" 2 2 left "  ")
                           ("File" 12 12 left bs--get-file-name))
      bs-dont-show-regexp (concat
                           "\\*\\("
                           "vc"
                           "\\|cvs-commit"
                           "\\|python-pylint\\|Pymacs"
                           "\\|Completions"
                           "\\)\\*"))

;;-----------------------------------------------------------------------------
;;; calc

(setq calc-display-trail nil)

(add-hook 'calc-mode-hook
          (lambda ()
            (local-set-key [kp-separator] 'calcDigit-start)
            ))

;;-----------------------------------------------------------------------------
;;; clojure

(defadvice nrepl-default-err-handler (after nrepl-focus-errors activate)
  "Focus the error buffer after errors, like Emacs normally does."
  (select-window (get-buffer-window "*nrepl-error*")))

(defadvice nrepl-eval-last-expression (after nrepl-flash-last activate)
  (if (fboundp 'slime-flash-region)
      (slime-flash-region (save-excursion (backward-sexp) (point)) (point))))

(defadvice nrepl-eval-expression-at-point (after nrepl-flash-at activate)
  (if (fboundp 'slime-flash-region)
      (apply #'slime-flash-region (nrepl-region-for-expression-at-point))))

;;-----------------------------------------------------------------------------
;;; compilation

(setq-default compilation-scroll-output t
              compilation-window-height 20
              compile-command (concat "gmake -C " default-directory " all"))

(add-hook 'compilation-mode-hook 'compilation-recenter-end-enable)
;;(add-hook 'compilation-mode-hook 'toggle-truncate-lines)

;;-----------------------------------------------------------------------------
;;; diff

(setq-default diff-switches "-uwd")

(eval-after-load 'diff-mode
  '(progn
     (define-key diff-mode-map (kbd "n") 'diff-hunk-next)
     (define-key diff-mode-map (kbd "p") 'diff-hunk-prev)))

;;-----------------------------------------------------------------------------
;;; emacs-lisp-mode

(use-package emacs-lisp-mode
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package elisp-slime-nav
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))
    (use-package ert
      :commands ert-run-tests-interactively
      :bind ("C-<f9>" . ert-run-tests-interactively)
      :config
      (progn
        (put 'ert-deftest 'lisp-indent-function 'defun)
        (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (font-lock-add-keywords
                     nil
                     '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                        (1 font-lock-keyword-face nil t)
                        (2 font-lock-function-name-face nil t))))))))
    (add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-mode))
  :bind (("M-&" . lisp-complete-symbol)))

;;-----------------------------------------------------------------------------
;;; expand-region

(use-package expand-region
  :commands (er/expand-region er/contract-region))

;;-----------------------------------------------------------------------------
;;; gdb

(setq-default gdb-many-windows t           ; gdb many windows
              gdb-use-separate-io-buffer t ; gdb stdio output
              gud-tooltip-mode t)          ; mouse hover variables

;;-----------------------------------------------------------------------------
;;; grep

(require 'grep-a-lot)

(grep-a-lot-setup-keys)

(setq grep-command "grep -nH -e ''")

(let ((find-command "find . \\( -path '*/CVS' -o -path '*/.hg' -o -path '*/.git' \\) -prune -o -type f -print0"))
  (if macosp
      (setq grep-find-command
            (concat find-command " | xargs -0 " grep-command))
    (setq grep-find-command
          (concat find-command " | xargs -0 -e " grep-command))))

(eval-after-load 'grep
  '(progn
     ;; Skip certain directories.
     (add-to-list 'grep-find-ignored-directories "target")
     (add-to-list 'grep-find-ignored-directories "vendor")
     ))

(add-hook 'grep-mode-hook 'setup--truncate-lines)

;;-----------------------------------------------------------------------------
;;; hl-line

(when (fboundp 'global-hl-line-mode)
  (require 'hl-line+)
  (global-hl-line-mode t))

;;-----------------------------------------------------------------------------
;;; isearch

(defun pabe/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))

(define-key isearch-mode-map (kbd "C-e") 'pabe/isearch-yank-current-word)

;;-----------------------------------------------------------------------------
;;; ispell

(setq ispell-program-name "aspell"
      ispell-local-dictionary "english"
      ispell-silently-savep t
      ispell-help-in-bufferp 'electric)

;;-----------------------------------------------------------------------------
;;; java

(add-hook 'java-mode-hook 'setup--java-mode)

(defun setup--java-mode ()
  (setq tab-width 4)
  (idle-highlight-mode))

;;-----------------------------------------------------------------------------
;;; Magit

(use-package magit
  :init
  (progn
    (use-package magit-blame)
    (defadvice magit-diff-working-tree (after magit-diff-focus activate)
      "After execution, select the magit-diff buffer in the current window."
      (other-window 1)))
  :config
  (progn
    (setq magit-status-buffer-switch-function 'switch-to-buffer
          magit-restore-window-configuration t
          magit-diff-refine-hunk nil))
  :bind ("C-x g" . magit-status))

;;-----------------------------------------------------------------------------
;;; Mercurial

(setq hg-commit-allow-empty-message t
      vc-hg-diff-switches "--text")

(defun setup--ahg ()
  (setup--truncate-lines)
  (local-set-key [tab] 'ahg-status-diff)
  (local-set-key [M-delete] 'ahg-status-unmark-all))

(eval-after-load "ahg-status"
  '(progn
     (add-hook 'ahg-status-mode-hook 'setup--ahg)
     ;; (remove-hook 'ahg-status-mode-hook 'setup--ahg)

     ;; ;; (ad-activate 'ahg-status)
     ;; ;; (ad-deactivate 'ahg-status)
     ;; ;; (ad-remove-advice 'ahg-status 'after 'ahg-status-fullscreen)
     ;; (defadvice ahg-status (after ahg-status-fullscreen activate)
     ;;   "After execution, show only the ahg-status buffer."
     ;;   (window-configuration-to-register :ahg-status-fullscreen)
     ;;   (switch-to-buffer "*aHg-status*" t t)
     ;;   (delete-other-windows))
     ))

;;-----------------------------------------------------------------------------
;;; minibuffer

(icomplete-mode 0)

(define-key minibuffer-local-map [up]     'previous-history-element)
(define-key minibuffer-local-map [down]   'next-history-element)
(define-key minibuffer-local-map [C-up]   'previous-complete-history-element)
(define-key minibuffer-local-map [C-down] 'next-complete-history-element)

;;-----------------------------------------------------------------------------
;;; rainbow-mode

(defun setup--rainbow-mode ()
  (rainbow-mode 1))

;;-----------------------------------------------------------------------------
;;; script

(use-package sh-script
  :config (setq sh-shell-file     "/bin/sh"
                sh-indentation    4
                sh-basic-offset   4
                sh-indent-comment t))

;;-----------------------------------------------------------------------------
;;; smartparens

(use-package smartparens
  :commands (smartparens-mode
             smartparens-strict-mode
             show-smartparens-mode)
  :config
  (progn
    (use-package smartparens-ruby)
    (use-package smartparens-html)
    (setq sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    (show-smartparens-global-mode 1)
    (sp--populate-keymap
     '(("C-<delete>" . sp-kill-sexp)
       ("C-<backspace>" . sp-backward-kill-sexp)
       ("C-M-w" . sp-copy-sexp)
       ("C-)" . sp-forward-slurp-sexp)
       ("C-}" . sp-forward-barf-sexp)
       ("C-(" . sp-backward-slurp-sexp)
       ("C-{" . sp-backward-barf-sexp)
       ("C-M-t" . sp-transpose-sexp)
       ("M-q" . sp-indent-defun)))))

;;-----------------------------------------------------------------------------
;;; sql

(setq plsql-indent 2)

(setq sql-interactive-mode-hook
      (lambda ()
        (setup--truncate-lines)
        (make-variable-buffer-local 'sql-input-ring-file-name)
        (setq sql-input-ring-file-name
              (expand-file-name
               (concat "history-" (symbol-name sql-product) ".sql")
               user-emacs-directory))
        (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
        (sql-rename-buffer)
;;         (case sql-product
;;           ("mysql"
;;            (require 'sql-completion)
;;            (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
;;            (sql-mysql-completion-init)))
        ))

(defun sql-make-smart-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.

This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'."
  (or (and (boundp 'sql-name) sql-name)
      (concat (if (not (string= "" sql-server))
                  (concat
                   (or (and (string-match "[0-9.]+" sql-server) sql-server)
                       (car (split-string sql-server "\\.")))
                   "/"))
              sql-database)))

;; (defadvice sql-connect-mysql (around sql-mysql-port activate)
;;   "Add support for connecting to MySQL on other ports"
;;   (let ((sql-mysql-options (or (and (boundp 'sql-port) sql-port (cons (concat "-P " (or (and (numberp sql-port) (number-to-string sql-port)) sql-port)) sql-mysql-options)) sql-mysql-options)))
;;     ad-do-it))

;;-----------------------------------------------------------------------------
;;; tramp

;; (require 'tramp nil t)

;; (setq tramp-completion-reread-directory-timeout nil)

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:"
                               (buffer-file-name (current-buffer)))))

;;-----------------------------------------------------------------------------
;;; truncate-lines

(defun setup--truncate-lines ()
  (toggle-truncate-lines 1))

;;-----------------------------------------------------------------------------
;;; VC

(require 'vc)

(setq vc-command-messages    t
      vc-follow-symlinks     t
      vc-consult-headers     t
      vc-keep-workfiles      t
      vc-make-backup-files   nil
      vc-dired-terse-display nil
      vc-dired-recurse       nil)

(add-hook 'cvs-mode-hook 'setup--cvs-mode)

(defun setup--cvs-mode ()
  (local-set-key [M-delete] 'cvs-mode-unmark-all-files)
  (local-set-key [return]   'cvs-mode-find-file))

;;-----------------------------------------------------------------------------
;;; webjump

(eval-after-load "webjump"
  '(progn
     (setq webjump-sites
           '(("Google" .
              [simple-query "www.google.com" "www.google.com/search?q=" ""])
             ("IMDB" .
              [simple-query "www.imdb.com" "www.imdb.com/Find?select=All&for=" ""])
             ("Wikipedia" .
              [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
             ("Urban Dictionary" .
              [simple-query "www.urbandictionary.com"
                            "http://www.urbandictionary.com/define.php?term=" ""])
             ("Python 2.6" .
              [simple-query "http://docs.python.org/2.6"
                            "http://docs.python.org/release/2.6/search.html?q=" ""])
             ("Google Maps" .
              [simple-query "maps.google.com"
                            "http://maps.google.com/maps?q=" ""])
             ("Open Street Map" .
              [simple-query "openstreetmap.org"
                            "http://nominatim.openstreetmap.org/search?q=" "&polygon=1"])
             ("Internet Drafts" .
              [simple-query
               "www.ietf.org/ID.html"
               ,(concat "search.ietf.org/cgi-bin/htsearch?restrict="
                        (webjump-url-encode "http://www.ietf.org/internet-drafts/")
                        "&words=") ""])
             ))))

;;-----------------------------------------------------------------------------
;;; whitespace

(require 'nuke-whitespace)

(add-hook 'write-file-hooks 'nuke-trailing-whitespace)

(add-to-list 'nuke-trailing-whitespace-always-major-modes 'ruby-mode)
(add-to-list 'nuke-trailing-whitespace-always-major-modes 'python-mode)

(setq whitespace-style '(trailing
                         ;;lines-tail
                         space-before-tab
                         space-after-tab
                         indentation
                         tab-mark)
      whitespace-line-column 80)

;;-----------------------------------------------------------------------------
;;; YAML

(defun yaml-next-field ()
  "Jump to next yaml field"
  (interactive)
  (search-forward-regexp ": *"))

(defun yaml-prev-field ()
  "Jump to next yaml field"
  (interactive)
  (search-backward-regexp ": *"))

;; (add-hook 'yaml-mode-hook 'flymake-yaml-load)
;; (remove-hook 'yaml-mode-hook 'flymake-yaml-load)

;; (add-hook 'yaml-mode-hook
;;           (lambda ()
;;             (define-key yaml-mode-map (kbd "C-<up>")    'yaml-prev-field)
;;             (define-key yaml-mode-map (kbd "C-<down>")  'yaml-next-field)
;;             ))

;;-----------------------------------------------------------------------------
;;; yasnippet

(use-package yasnippet
  :init
  (progn
    (setq yas-verbosity 1
          yas-wrap-around-region t
          yas-fallback-behavior 'return-nil)
    (setq-default yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
    (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)
    (let ((snippets-dir (expand-file-name "snippets" user-emacs-directory)))
      (yas-load-directory snippets-dir)
      (setq yas-snippet-dirs snippets-dir))))

;;-----------------------------------------------------------------------------

(setq next-line-add-newlines nil) ;; Don't add newlines with 'down' key.

(global-font-lock-mode 1)
(blink-cursor-mode -1)
(auto-compression-mode 1)
(winner-mode 1)

(when (require 'framemove nil t)
  (windmove-default-keybindings 'shift)
  ;; Cannot wrap and have framemove do its thing at the same time.
  (setq windmove-wrap-around nil
        framemove-hook-into-windmove t))

(fset 'yes-or-no-p 'y-or-n-p)

(if (assoc "UTF-8" language-info-alist)
    (progn
      (set-language-environment "utf-8")
      (prefer-coding-system 'utf-8))
  (set-language-environment 'Latin-1)
  (set-input-mode t nil 'iso)
  (standard-display-8bit 160 255)
  ;;(standard-display-european t)
  (unify-8859-on-decoding-mode))

(add-hook 'find-file-hook 'pabe-find-file-follow-symlink)

;;; Set `completion-ignored-extensions'
(add-to-list 'completion-ignored-extensions ".ali")
(add-to-list 'completion-ignored-extensions ".adt")
(add-to-list 'completion-ignored-extensions ".class")
(add-to-list 'completion-ignored-extensions ".rbc")

(setq completion-ignored-extensions
      (remove ".pdf" completion-ignored-extensions))
(setq completion-ignored-extensions
      (remove ".bin" completion-ignored-extensions))
(setq completion-ignored-extensions
      (remove ".lib" completion-ignored-extensions))
(setq completion-ignored-extensions
      (remove ".log" completion-ignored-extensions))

(setq line-move-visual nil) ; Make Emacs 23 move like old emacs

(setq default-major-mode 'indented-text-mode)

(setq save-abbrevs nil)

(setq-default fill-column 72)

(setq mouse-yank-at-point t)
(setq yank-excluded-properties t) ; do not paste any properties

;; New in Emacs 23.2.
(setq make-pointer-invisible t)

(setq print-escape-newlines t)
(setq garbage-collection-messages nil)

(line-number-mode 1)
(column-number-mode 1)

(setq lpr-command          "lpr"
      lpr-headers-switches "-h")

(setq-default auto-save-default             nil
              backup-inhibited              t
              confirm-kill-emacs            'y-or-n-p
              delete-active-region          nil
              echo-keystrokes               0.1
              indent-tabs-mode              nil
              make-backup-files             nil
              require-final-newline         nil
              standard-indent               4
              trim-versions-without-asking  t
              version-control               nil
              visible-bell                  t
              x-stretch-cursor              t)

(setq scroll-step 0
      scroll-conservatively 0)

;; Don't truncate lines
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; Trash can support.
(setq delete-by-moving-to-trash nil)

(setq sentence-end-double-space nil)    ; make M-[ae] work
(setq sentence-end "[.?!][]\"')]*\\($\\|\t\\| \\)[ \t\n]*")

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(if (>= emacs-major-version 22)
    (progn
      (setq cua-enable-cua-keys   nil ;; only for rectangles
            cua-delete-selection  nil)
      (cua-mode t)))

(Xlaunch
 (setq frame-title-format
       '((:eval (if (and (buffer-file-name) (file-regular-p (buffer-file-name)))
                    "%*"
                  ""))
         (:eval (or (buffer-name) "%b"))
         " <" user-login-name "%@" (:eval (system-name)) ">"))

 (setq-default buffers-menu-max-size nil)

 ;; Copy-paste should work with other X clients.
 (when (>= emacs-major-version 24)
   (setq select-active-regions t
         x-select-enable-clipboard t
         x-select-enable-primary t
         save-interprogram-paste-before-kill t
         interprogram-cut-function 'x-select-text
         interprogram-paste-function 'x-selection-value))

 (set-default 'indicate-empty-lines t)
 (set-default 'indicate-buffer-boundaries 'left)

 (if (boundp 'transient-mark-mode)
     (transient-mark-mode t))
 (setq mark-even-if-inactive t)

 (require 'mwheel)
 (mwheel-install)
 )

(use-package saveplace
  :config
  (progn
    (setq-default save-place t)
    (setq save-place-file (expand-file-name ".places" user-emacs-directory))))

;;; Misc. setup
(setq flyspell-issue-welcome-flag nil)

(setq toe-starting-length         3
      toe-starting-time-per-word 10
      toe-max-length             20)

(when (fboundp 'savehist-mode)
  (setq savehist-ignored-variables '(file-name-history))
  (savehist-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(which-func-mode 1) ; Enable mode in any major mode that supports it.

(fset 'man 'woman)

(setq woman-use-own-frame nil
      woman-fontify       t
      woman-imenu         t
      Man-notify-method   'pushy)

(setq reb-re-syntax 'string)

(when (require 'openwith nil 'noerror)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
               (if macosp "open" "mplayer")
               '(file))
         (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg" "jpg"))
               (if macosp "open" "eog")
               '(file))
         (list (openwith-make-extension-regexp
                '("pdf")) ; "ps" "ps.gz" "dvi"))
               (if macosp "open" "evince")
               '(file))
         (list (openwith-make-extension-regexp
                '("epub"))
               (if macosp "open" "FBReader")
               '(file))
         ))
  (openwith-mode 1))

(when (require 'printing nil t) ; load printing package if available
  (pr-update-menus t)) ; update now printer and utility menus

(when (fboundp 'mac-print-mode)
  (mac-print-mode 1)
  ;;(global-set-key (kbd "M-p") 'mac-print-buffer)
  )

;; (setq pr-path-alist
;;       '((unix      "." "~/bin" ;ghostview mpage PATH)
;;         (ghostview "$HOME/bin/gsview-dir")
;;         (mpage     "$HOME/bin/mpage-dir")
;;         ))
;; (setq pr-txt-name 'prt_06a)
;; (setq pr-txt-printer-alist
;;       '((prt_06a "lpr" nil "prt_06a")
;;         (prt_07c nil   nil "prt_07c")
;;         ))
;; (setq pr-ps-name 'lps_06b)
;; (setq pr-ps-printer-alist
;;       '((lps_06b "lpr" nil "-P" "lps_06b")
;;         (lps_07c "lpr" nil nil  "lps_07c")
;;         (lps_08c nil   nil nil  "lps_08c")
;;         ))


;; ;;(require 'ps-print)

(setq ps-paper-type         'a4
      ps-print-color-p      nil
      ps-number-of-columns  2
      ps-landscape-mode     t)
