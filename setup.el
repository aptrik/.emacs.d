(use-package ahg
  :defer t
  :commands (ahg-status)
  :config
  (progn
    (add-hook 'ahg-status-mode-hook 'turn-on-truncate-lines)
    (bind-keys ahg-status-mode-map
               '(("<tab>" . ahg-status-diff)
                 ("M-<delete>" . ahg-status-unmark-all)))))


(use-package arc-mode
  :defer t
  :mode (("\\.egg$" . archive-mode)
         ("\\.\\(war\\|jar\\)$" . archive-mode))
  :config
  (progn
    (add-hook 'archive-mode-hook 'turn-on-truncate-lines)))


(use-package bookmark
  :defer t
  :config
  (progn
    (setq bookmark-save-flag 1
          bmkp-last-as-first-bookmark-file nil)
    (use-package bookmark+)))


(use-package browse-kill-ring
  :defer t
  :commands (browse-kill-ring)
  :config
  (setq browse-kill-ring-quit-action 'save-and-restore)
  :bind ("C-c y" . browse-kill-ring))


(use-package bs
  :defer t
  :config
  (progn
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
                               "\\)\\*")))
  :bind (("C-<escape>" . bs-show)
         ("C-<tab>" . bs-show)))


(use-package calc
  :defer t
  :config
  (progn
    (setq calc-display-trail nil)
    (add-hook 'calc-mode-hook
              (lambda ()
                (local-set-key [kp-separator] 'calcDigit-start)))))


(use-package calendar
  :defer t
  :config
  (progn
    (european-calendar)

    (add-hook 'diary-display-hook          'fancy-diary-display)
    (add-hook 'today-visible-calendar-hook 'calendar-mark-today)
    (add-hook 'list-diary-entries-hook     'sort-diary-entries t)

    (setq calendar-location-name           "Göteborg, SE"
          calendar-latitude                57.72 ; 57° 43' North
          calendar-longitude               11.97 ; 11° 58' East
          calendar-week-start-day          1
          calendar-offset                  0
          calendar-today-marker            'calendar-today-face
          view-calendar-holidays-initially nil
          calendar-date-display-form       '((format "%s-%02d-%02d"
                                                     year
                                                     (string-to-number month)
                                                     (string-to-number day)))
          view-diary-entries-initially     t
          mark-holidays-in-calendar        t
          mark-diary-entries-in-calendar   t
          calendar-time-display-form       '(24-hours ":" minutes)
          diary-display-hook               'fancy-diary-display
          holidays-in-diary-buffer         t
          diary-list-include-blanks        t
          diary-file                       (expand-file-name "~/.diary")
          calendar-day-name-array
          ["Söndag" "Måndag" "Tisdag" "Onsdag" "Torsdag" "Fredag" "Lördag"]
          calendar-month-name-array
          ["Januari" "Februari" "Mars" "April" "Maj" "Juni"
           "Juli" "Augusti" "September" "Oktober" "November" "December"]
          all-christian-calendar-holidays nil
          general-holidays                nil
          oriental-holidays               nil
          hebrew-holidays                 nil
          islamic-holidays                nil
          swedish-holidays
          ;; Se: http://www.kalender.se
          ;; och: http://hem.passagen.se/farila/holiday.htm
          '((holiday-fixed  1  1    "Nyårsdagen")
            (holiday-fixed  1  6    "Trettondedag jul")
            (holiday-fixed  2 14    "Alla hjärtans dag")
            (holiday-fixed  4 30    "Valborgmässoafton")
            (holiday-fixed  5  1    "Första maj/Valborg")
            (holiday-float  5  0 -1 "Mors dag") ; Sista söndagen i maj
            (holiday-fixed  6  6    "Sveriges nationaldag (svenska flaggans dag)")
            ;; Midsommardagen 2005-06-25
            (holiday-float 11  6  1 "Alla helgons dag") ; Första lördagen i nov.
            (holiday-float 11  0  2 "Fars dag") ; Andra söndagen i november
            (holiday-fixed 12 10    "Nobeldagen")
            (holiday-fixed 12 25    "Juldagen")
            (holiday-fixed 12 26    "Annandag jul")
            (holiday-advent 0 "Första advent")
            (holiday-easter-etc -2 "Långfredag")
            (holiday-easter-etc 0  "Påskdagen")
            (holiday-easter-etc 1  "Annandag påsk")
            (holiday-easter-etc 39 "Kristi himmelsfärds dag")
            (holiday-easter-etc 49 "Pingstdagen")
            (holiday-easter-etc 50 "Annandag pingst"))
          calendar-holidays
          (append
           general-holidays
           local-holidays
           other-holidays
           christian-holidays
           solar-holidays
           swedish-holidays))))


(use-package cc-mode
  :defer t
  :config
  (progn
    (add-hook 'c-mode-hook 'setup--c-mode-hook)
    (add-hook 'c++-mode-hook 'setup--c-mode-hook)
    
    (defconst setup--c-style
      `((c-recognize-knr-p . nil)
        (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
        ;;(c-echo-syntactic-information-p . t)
        ;;(c-basic-offset . 2)
        (c-comment-only-line-offset 0 . 0)
        (c-tab-always-indent . t)
        (c-indent-comments-syntactically-p . nil)
        (comment-column . 40)

        (c-cleanup-list . (;;brace-else-brace
                           ;;brace-elseif-brace
                           ;;brace-catch-brace
                           empty-defun-braces
                           defun-close-semi
                           list-close-comma
                           scope-operator))

        (c-offsets-alist . ((arglist-intro         . c-lineup-arglist-intro-after-paren)
                            (arglist-close         . c-lineup-arglist)
                            (brace-list-open       . +)
                            (case-label            . +)
                            (func-decl-cont        . c-lineup-java-throws)
                            (inher-cont            . c-lineup-java-inher)
                            (inline-open           . 0)
                            (knr-argdecl           . 0)
                            (knr-argdecl-intro     . 0)
                            (stream-op             . c-lineup-streamop)
                            (label                 . *)
                            (statement-block-intro . +)
                            (statement-case-open   . +)
                            (statement-cont
                             .
                             (,(when (fboundp 'c-no-indent-after-java-annotations)
                                 'c-no-indent-after-java-annotations)
                              ,(when (fboundp 'c-lineup-assignments)
                                 'c-lineup-assignments)
                              +))
                            (substatement-label    . 0)
                            (substatement-open     . +)
                            )))
      "C/C++ programming style.")

    (defun setup--set-c-style ()
      "Set the current buffer's c-style."
      (interactive)
      (make-local-variable 'c-tab-always-indent)
      (setq c-tab-always-indent t)
      (c-add-style "c-c++-style" setup--c-style t)
      (setq c-basic-offset 2))

    (defun setup--c-mode-hook ()
      (which-function-mode 1)
      (subword-mode 1)

      (setup--set-c-style)

      (set (make-local-variable 'compile-command)
           (concat "gmake -C " default-directory " all"))

      (c-toggle-electric-state 1)
      (c-toggle-auto-newline 1)
      ;; (c-toggle-hungry-state 1)
      (abbrev-mode 0)

      (local-set-key (kbd "C-c o") 'ff-find-other-file)
      (local-set-key (kbd "C-c c") 'compile)
      (local-set-key (kbd "C-c C-c") 'recompile)))
  :mode (("\\.[ch]$" . c-mode)
         ("\\.\\(cc\\|hh\\)$" . c++-mode)
         ("\\.\\(i\\|swg\\)$" . c++-mode)))


(use-package cperl-mode
  :defer t
  :config
  (progn
    (defalias 'perl-mode 'cperl-mode)
    (fset 'perldoc 'cperl-perldoc)
    (setq interpreter-mode-alist
          (append '(("perl"     . perl-mode)
                    ("perl5"    . perl-mode)
                    ("miniperl" . perl-mode))
                  interpreter-mode-alist))

    (setq perl-indent-level                2
          perl-continued-statement-offset  2
          perl-continued-brace-offset      0
          perl-brace-offset                0
          perl-brace-imaginary-offset      0
          perl-label-offset               -2)

    (setq cperl-indent-level                2
          cperl-continued-statement-offset  2
          cperl-continued-brace-offset      0
          cperl-brace-offset                0
          cperl-brace-imaginary-offset      0
          cperl-label-offset               -2
          cperl-electric-keywords           t
          cperl-electric-parens             nil
          cperl-merge-trailing-else         nil
          cperl-under-as-char               t
          cperl-invalid-face                nil)))


(use-package css-mode
  :defer t
  :config
  (progn
    (setq cssm-indent-function 'cssm-c-style-indenter
          css-indent-level 2
          scss-compile-at-save nil)
    (add-hook 'css-mode-hook 'turn-on-rainbow-mode))
  :mode (("\\.css$" . css-mode)))


(use-package compile
  :defer t
  :config
  (progn
    (setq-default compilation-scroll-output t
                  compilation-window-height 20
                  compile-command (concat "gmake -C " default-directory " all"))

    ;;(add-hook 'compilation-mode-hook 'toggle-truncate-lines)
    (add-hook 'compilation-mode-hook 'compilation-recenter-end-enable)))


(use-package diff-mode
  :defer t
  :commands diff-mode
  :config
  (progn
    (setq-default diff-switches "-uwd")
    (define-key diff-mode-map (kbd "n") 'diff-hunk-next)
    (define-key diff-mode-map (kbd "p") 'diff-hunk-prev)))


(use-package dired
  :defer t
  :config
  (progn
    (setq dired-details-hidden-string ""
          dired-details-hide-link-targets nil
          dired-details-initially-hide nil
          dired-dwim-target t
          dired-omit-files "^\\.[^.]"
          dired-recursive-copies 'always
          dired-recursive-deletes 'top
          wdired-allow-to-change-permissions t)
    (use-package dired-x)
    (use-package dired-sort-menu+)
    (use-package dired-details
      :config (dired-details-install))
    (defun setup--dired-mode ()
      (dired-omit-mode 1)
      (turn-on-truncate-lines)
      (local-set-key (kbd "M-o") 'dired-omit-mode)
      (local-set-key (kbd "T")   'dired-do-touch))
    (add-hook 'dired-mode-hook 'setup--dired-mode)

    (unless (boundp 'dired-guess-shell-alist-user)
      (setq dired-guess-shell-alist-user '()))

    (cond
     (linuxp
      (add-to-list 'dired-guess-shell-alist-user
                   '("\\.\\(jpe?g\\|gif\\|png\\)\\'" "feh -drF --sort filename -D5 * &"))
      (add-to-list 'dired-guess-shell-alist-user
                   '("\\.\\(avi\\|mpg\\|wmv\\|mp4\\|mov\\|m4v\\)\\'" "mplayer -really-quiet * &"))
      (add-to-list 'dired-guess-shell-alist-user
                   '("\\.pdf\\'" "acroread * &" "evince * &"))
      (add-to-list 'dired-guess-shell-alist-user
                   '("\\.epub\\'" "FBReader * &" "evince * &"))))))


(use-package ediff
  :defer t
  :config
  (progn
    (setq-default ediff-ignore-similar-regions t)
    (setq ediff-window-setup-function 'ediff-setup-windows-plain
          ediff-split-window-function (lambda (&optional arg)
                                        (if (> (frame-width) 150)
                                            (split-window-horizontally arg)
                                          (split-window-vertically arg))))))


(use-package edit-env
  :defer t)


(use-package expand-region
  :bind (("C-+" . er/expand-region)
         ("C-?" . er/contract-region)))


(use-package framemove
  :init
  (progn
    (windmove-default-keybindings 'shift)
    ;; Cannot wrap and have framemove do its thing at the same time.
    (setq windmove-wrap-around nil
          framemove-hook-into-windmove t)))


(use-package grep
  :defer t
  :bind (("M-s D" . find-dired)
         ("M-s d" . find-grep-dired)
         ("M-s f" . find-grep)
         ("M-s g" . grep)
         ("M-s n" . find-name-dired)
         ("M-s r" . rgrep))
  :config
  (progn
    (grep-apply-setting 'grep-command "egrep -nH -e ")
    (grep-apply-setting
     'grep-find-command
     '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52))

    (add-hook 'grep-mode-hook 'turn-on-truncate-lines)

    (add-to-list 'grep-find-ignored-directories "target")
    (add-to-list 'grep-find-ignored-directories "vendor")

    (use-package grep-a-lot
      :config (grep-a-lot-setup-keys))

    (let ((find-command "find . \\( -path '*/CVS' -o -path '*/.hg' -o -path '*/.git' \\) -prune -o -type f -print0"))
      (if macosp
          (setq grep-find-command
                (concat find-command " | xargs -0 " grep-command))
        (setq grep-find-command
              (concat find-command " | xargs -0 -e " grep-command))))))


(use-package gud
  :defer t
  :config
  (setq-default gdb-many-windows t
                gdb-use-separate-io-buffer t
                gud-tooltip-mode t))


(use-package hl-line
  :init
  (progn
    (use-package hl-line+)
    (global-hl-line-mode t)))


(use-package hl-tags-mode
  :defer t)


(when (fboundp 'isearch-mode)
  (defun pabe/isearch-yank-current-word ()
    "Pull current word from buffer into search string."
    (interactive)
    (save-excursion
      (skip-syntax-backward "w_")
      (isearch-yank-internal
       (lambda ()
         (skip-syntax-forward "w_")
         (point)))))
  (bind-keys isearch-mode-map
             '(("C-e" . pabe/isearch-yank-current-word)
               ("C-c" . isearch-toggle-case-fold)
               ("C-t" . isearch-toggle-regexp)
               ("C-^" . isearch-edit-string))))


(use-package ispell
  :defer t
  :bind (("C-c s b" . ispell-buffer)
         ("C-c s c" . ispell-comments-and-strings)
         ("C-c s d" . ispell-change-dictionary)
         ("C-c s r" . ispell-region)
         ("C-c s w" . ispell-word))
  :config
  (setq ispell-program-name "aspell"
        ispell-local-dictionary "english"
        ispell-silently-savep t
        ispell-help-in-bufferp 'electric))


;; (use-package java-mode
;;   :config
;;   (progn
;;     (defun setup--java-mode ()
;;       (setq tab-width 4)
;;       (idle-highlight-mode))
;;     (add-hook 'java-mode-hook #'setup--java-mode)

;;     (use-package generic-x
;;       :commands (java-manifest-generic-mode
;;                  java-properties-generic-mode)
;;       :mode (("MANIFEST.MF\\'" . java-manifest-generic-mode)
;;              ("\\.prop\\'" . java-properties-generic-mode)))))


(use-package js2-mode
  :defer t
  :config
  (progn
    (setq js2-basic-offset 4
          ;;js2-skip-preprocessor-directives t
          js2-use-font-lock-faces t)
    (setq-default js2-additional-externs
                  '("$" "unsafeWindow" "localStorage" "jQuery"
                    "setTimeout" "setInterval" "location" "console")))
  :mode ("\\.js$" . js2-mode))


(use-package lisp-mode
  :defer t
  :config
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package elisp-slime-nav
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))
    (use-package ert
      :defer t
      :commands ert-run-tests-interactively
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
    (use-package smartparens)
    ;;(add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-mode)
    (bind-keys emacs-lisp-mode-map
               '(("C-<f9>" . ert-run-tests-interactively)
                 ("M-&" . lisp-complete-symbol)
                 ("C-<delete>" . sp-kill-sexp)
                 ("C-<backspace>" . sp-backward-kill-sexp)
                 ("C-M-w" . sp-copy-sexp)
                 ("C-)" . sp-forward-slurp-sexp)
                 ("C-}" . sp-forward-barf-sexp)
                 ("C-(" . sp-backward-slurp-sexp)
                 ("C-{" . sp-backward-barf-sexp)
                 ("C-M-t" . sp-transpose-sexp)
                 ("M-q" . sp-indent-defun)))

    (defun setup--emacs-lisp-mode ()
      (add-hook 'after-save-hook 'check-parens nil t))

    (add-hook 'emacs-lisp-mode-hook 'setup--emacs-lisp-mode))
  :mode ("Cask" . emacs-lisp-mode))


(use-package magit
  :defer t
  :config
  (progn
    (setq magit-status-buffer-switch-function 'switch-to-buffer
          magit-restore-window-configuration t
          magit-diff-refine-hunk nil)
    (use-package magit-blame)
    (defadvice magit-diff-working-tree (after magit-diff-focus activate)
      "After execution, select the magit-diff buffer in the current window."
      (other-window 1)))
  :bind ("C-x g" . magit-status))


(use-package markdown-mode
  :mode (("README\\.md$" . gfm-mode)))


(use-package nuke-whitespace
  :defer t
  :config
  (progn
    (add-hook 'write-file-hooks 'nuke-trailing-whitespace)
    (add-to-list 'nuke-trailing-whitespace-always-major-modes 'ruby-mode)
    (add-to-list 'nuke-trailing-whitespace-always-major-modes 'python-mode)))


(use-package nxml-mode
  :defer t
  :config
  (progn
    (setq nxml-auto-insert-xml-declaration-flag nil
          nxml-bind-meta-tab-to-complete-flag t
          nxml-child-indent 2
          nxml-slash-auto-complete-flag t
          nxml-syntax-highlight-flag t
          rng-nxml-auto-validate-flag nil)
    (push '("<\\?xml" . nxml-mode) magic-mode-alist)
    (add-hook 'sgml-mode-hook 'turn-on-hl-tags-mode)
    (add-hook 'nxml-mode-hook 'turn-on-hl-tags-mode)))


(use-package openwith
  :defer t
  :config
  (progn
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
                 '(file))))
    (openwith-mode 1)))


(use-package org
  :defer t
  :config
  (progn
    (setq
     ;;org-default-notes-file     "~/Dropbox/TODO"
     ;;org-directory              "~/org"
     org-clock-continuously     nil
     org-clock-idle-time        nil
     org-clock-in-resume        t
     org-clock-persist          'history
     org-hide-leading-stars     nil
     org-level-color-stars-only t
     org-log-done               'time
     org-odd-levels-only        nil
     org-replace-disputed-keys  t
     org-reverse-note-order     t
     org-src-fontify-natively   t
     org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
    (org-clock-persistence-insinuate)
    (add-hook 'org-mode-hook 'setup--org-mode)
    (defun setup--org-mode ()
      (setq org-blank-before-new-entry '((heading . t)
                                         (plain-list-item . nil)))

      ;; Set program to use when opening PDF files.
      (if macosp
          (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
        (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))

      (local-set-key [M-up]          'outline-previous-visible-heading)
      (local-set-key [M-down]        'outline-next-visible-heading)
      (local-set-key [(control tab)] 'bs-show))
    (setq org-capture-templates
          '(("t" "Task" entry
             (file+headline"~/Dropbox/TODO" "Incoming")
             "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00|8:00}
:END:" :empty-lines-after 2)
            ("m" "Music" item
             (file+headline "~/Dropbox/TODO" "Music")
             "- [ ] %^{Music}" :empty-lines-after 2)
            ("M" "Movie/TV show" item
             (file+headline "~/Dropbox/TODO" "Movie/TV")
             "- [ ] %^{Movie/TV show}" :empty-lines-after 2))))
  :bind (("M-m" . org-capture))
  :mode ("\\.org$" . org-mode))


(use-package paren
  :init
  (progn
    (show-paren-mode t)
    (setq show-paren-style 'parenthesis)))


(use-package php-mode
  :defer t
  :config
  (progn
    (setq php-extra-constants '())
    (add-hook 'php-mode-hook 'setup--php-mode)

    (defun setup--php-mode()
      "PEAR/PHP setup."
      (setq case-fold-search t)
      (setq indent-tabs-mode nil)
      (setq fill-column 78)

      (subword-mode 1)

      (setq c-electric-flag nil)
      (setq c-basic-offset 4)

      (c-set-offset 'arglist-intro '+)
      (c-set-offset 'arglist-close '0)
      (c-set-offset 'case-label 2)

      (setq php-warned-bad-indent t)

      (local-set-key [C-f7] 'php-lint)
      (local-set-key [f7] 'phpcs))

    (defun php-lint ()
      "Performs a PHP lint check on the current file."
      (interactive)
      (let ((compilation-error-regexp-alist '(php))
            (compilation-error-regexp-alist-alist ()))
        (pushnew '(php "\\(syntax error.*\\) in \\(.*\\) on line \\([0-9]+\\)$" 2 3 nil nil 1)
                 compilation-error-regexp-alist-alist)
        (compile (concat "php -l -f \"" (buffer-file-name) "\""))))

    (defun phpcs ()
      "Performs a PHP code sniffer check on the current file."
      (interactive)
      (let ((compilation-error-regexp-alist '(php))
            (compilation-error-regexp-alist-alist ()))
        (pushnew '(php "\"\\([^\"]+\\)\",\\([0-9]+\\),\\([0-9]+\\),\\(warning\\|error\\),\\(.*\\)" 1 2 3 (5 . 6) 4)
                 ;; (pushnew '(php "\"\\([^\"]+\\)\",\\([0-9]+\\),\\([0-9]+\\),\\(\\(warning\\)\\|\\(error\\)\\),\\(.*\\)" 1 2 3 (5 . 6) 4)
                 compilation-error-regexp-alist-alist)
        ;; See:
        ;; * http://pear.php.net/manual/en/standards.php
        ;; * http://pear.php.net/manual/en/package.php.php-codesniffer.annotated-ruleset.php
        (compile (concat "phpcs"
                         " --standard=Zend"
                         " --report=csv"
                         " \"" (buffer-file-name) "\""))))))


(use-package printing
  :defer t
  :config
  (progn
    (pr-update-menus t)
    (setq lpr-command          "lpr"
          lpr-headers-switches "-h"
          ps-paper-type         'a4
          ps-print-color-p      nil
          ps-number-of-columns  2
          ps-landscape-mode     t)))


(use-package python-mode
  :defer t
  :config
  (progn
    (setq jedi:complete-on-dot nil)

    (modify-syntax-entry ?\_ "_" python-mode-syntax-table)

    (setenv "PYTHONPATH" (concat (if (getenv "PYTHONPATH") "$PYTHONPATH:" "")
                                 (expand-file-name "bin/lib/python" user-emacs-directory))
            t)

    (require 'tramp) ;; needed by pep8 and pylint
    (require 'python-pylint)
    (require 'python-pep8)

    (defadvice pdb (before gud-query-cmdline activate)
      "Provide a better default command line when called interactively."
      (interactive
       (list (gud-query-cmdline 'pdb
                                (file-name-nondirectory buffer-file-name)))))

    (add-hook 'python-mode-hook 'setup--python-mode)

    (defun py-run ()
      "Run python on the file in the current buffer."
      (interactive)
      (compile (format "python \"%s\"" (buffer-file-name))))

    (defun setup--python-mode ()
      (which-function-mode 1)
      (subword-mode 1)

      ;;(setq py-python-command-args '( "-colors" "Linux"))

      (jedi:setup)

      (let ((map jedi-mode-map))
        (define-key map (kbd "<C-tab>") 'bs-show)
        (define-key map (kbd "<M-tab>") 'jedi:complete)
        (define-key map (kbd "M-.")     'jedi:goto-definition)
        (define-key map (kbd "C-.")     'jedi:complete)
        (define-key map (kbd "M-,")     'jedi:goto-definition-pop-marker))

      ;;(whitespace-mode 1)
      ;;(turn-on-eldoc-mode) ; doesn't work with python-mode from https://launchpad.net/python-mode

      (set (make-variable-buffer-local 'outline-regexp) "def\\|class ")
      (set (make-variable-buffer-local 'indent-tabs-mode) nil)

      (local-set-key (kbd "C-c c") 'compile)
      (local-set-key (kbd "C-c C-c") 'recompile)

      (let ((map subword-mode-map))
        (define-key map [M-left]       'subword-backward)
        (define-key map [M-right]      'subword-forward)
        (define-key map [C-left]       'subword-backward)
        (define-key map [C-right]      'subword-forward)
        (define-key map [C-backspace]  'subword-backward-kill))

      (local-set-key [C-M-up]   'py-beginning-of-block)
      (local-set-key [C-M-down] 'py-end-of-def-or-class)

      (local-set-key [M-up]   'py-beginning-of-def-or-class)
      (local-set-key [M-down] 'py-end-of-def-or-class)

      (local-set-key [f7] 'python-pylint)
      (local-set-key [C-f7] 'python-pep8)

      (local-set-key [f9]   'py-run)
      (local-set-key [S-f9] 'pdb) ; defined in gud
      (local-set-key [C-f9] 'compile)
      (local-set-key [M-f9] 'recompile))))


(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))


(use-package ruby-mode
  :defer t
  :config
  (progn
    (use-package ruby-end)
    ;;(use-package smartparens-ruby)

    (add-hook 'ruby-mode-hook 'setup--ruby-mode)

    (defun setup--ruby-mode ()
      (rvm-activate-corresponding-ruby)
      (robe-mode 1)
      (which-function-mode 1)
      ;;(show-smartparens-mode 1)

      (make-variable-buffer-local 'compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist
            (append compilation-error-regexp-alist
                    (list (list
                           (concat
                            "\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\)"
                            ":\\([0-9]+\\)") 2 3))))

      (make-variable-buffer-local 'compile-command)
      (setq compile-command (concat "ruby " (buffer-file-name) " "))

      (local-set-key (kbd "C-c c") 'compile)
      (local-set-key (kbd "C-c C-c") 'recompile)

      (local-set-key [f9]    'ruby-run)
      (local-set-key [C-f9]  'ruby-test)
      (local-set-key [M-f9]  'ri))

    (defun ruby-run ()
      "Run ruby on the file in the current buffer."
      (interactive)
      (compile (concat "ruby " (buffer-file-name))))

    (defun ruby-test ()
      "Run testrb on the file in the current buffer."
      (interactive)
      (compile (concat "testrb " (file-name-directory (buffer-file-name))))))
  :mode (("Gemfile$" . ruby-mode)
         ("Rakefile$" . ruby-mode)
         ("Guardfile" . ruby-mode)
         ("Vagrantfile$" . ruby-mode)
         ("\\.watchr$" . ruby-mode)
         ("\\.rake$" . ruby-mode)
         ("\\.rb$" . ruby-mode)))


(use-package sh-script
  :defer t
  :config
  (setq sh-shell-file     "/bin/sh"
        sh-indentation    4
        sh-basic-offset   4
        sh-indent-comment t)
  :mode (("/\\.\\(my\\)?login$" . sh-mode)
         ("/\\.\\(my\\)?logout$" . sh-mode)
         ("/\\.\\(my\\)?t?cshrc$" . sh-mode)
         ("/\\.profile$" . sh-mode)
         ("/\\.xinitrc$" . sh-mode)
         ("\\.t?c?sh$" . sh-mode)))


(use-package shell
  :defer t
  :config
  (setq explicit-shell-file-name "bash"
        shell-file-name shell-file-name
        shell-command-switch "-c"))


(use-package shell-toggle
  :bind ("M-z" . shell-toggle))


(use-package savehist
  :init
  (progn
    (setq savehist-ignored-variables '(file-name-history))
    (savehist-mode 1)))


(use-package saveplace
  :init
  (progn
    (setq-default save-place t)
    (setq save-place-file (expand-file-name ".places" user-emacs-directory))))


(use-package scss-mode
  :defer t
  :config
  (progn
    (add-hook 'scss-mode-hook 'turn-on-rainbow-mode))
  :mode (("\\.scss$" . scss-mode)))


(use-package smartparens
  :defer t
  :commands (smartparens-mode
             smartparens-strict-mode
             show-smartparens-mode)
  :config
  (progn
    ;; (use-package smartparens-ruby)
    ;; (use-package smartparens-html)
    (setq sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    ;;(show-smartparens-global-mode 1)
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


(use-package sql
  :defer t
  :config
  (progn
    (setq plsql-indent 2)
    (add-hook 'sql-interactive-mode-hook 'setup--sql-interactive-mode)

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
    (defun setup--sql-interactive-mode ()
      (turn-on-truncate-lines)
      (make-variable-buffer-local 'sql-input-ring-file-name)
      (setq sql-input-ring-file-name
            (expand-file-name
             (concat "history-" (symbol-name sql-product) ".sql")
             user-emacs-directory))
      (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
      (sql-rename-buffer))))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package vc
  :defer t
  :config
  (progn
    (setq vc-command-messages    t
          vc-follow-symlinks     t
          vc-consult-headers     t
          vc-keep-workfiles      t
          vc-make-backup-files   nil
          vc-dired-terse-display nil
          vc-dired-recurse       nil)
    (use-package vc-hg
      :config
      (setq hg-commit-allow-empty-message t
            vc-hg-diff-switches "--text"))

    (define-key vc-prefix-map "e" 'ediff-revision-current-buffer)

    (defun setup--cvs-mode ()
      (local-set-key [M-delete] 'cvs-mode-unmark-all-files)
      (local-set-key [return]   'cvs-mode-find-file))
    (add-hook 'cvs-mode-hook 'setup--cvs-mode)))


(use-package web-mode
  :defer t
  :config
  (progn
    (setq web-mode-enable-block-face t
          web-mode-enable-part-face t
          web-mode-disable-css-colorization nil
          web-mode-disable-auto-pairing nil))
  :mode (("\\.rhtml$" . web-mode)
         ("\\.\\(php\\|inc\\)$" . web-mode)))


(use-package webjump
  :defer t
  :config
  (progn
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
                       "&words=") ""])))))


(use-package woman
  :defer t
  :config
  (progn
    (fset 'man 'woman)
    (setq woman-use-own-frame nil
          woman-fontify       t
          woman-imenu         t
          Man-notify-method   'pushy)))


(use-package yaml-mode
  :mode ("\\.ya?ml$" . yaml-mode))


(use-package yasnippet
  :defer t
  :config
  (progn
    (setq yas-verbosity 1
          yas-wrap-around-region t
          yas-fallback-behavior 'return-nil)
    (setq-default yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
    (bind-key "<return>" 'yas-exit-all-snippets yas-keymap)
    (let ((snippets-dir (expand-file-name "snippets" user-emacs-directory)))
      (yas-load-directory snippets-dir)
      (setq yas-snippet-dirs snippets-dir)))
  :bind (("C-'" . yas-expand-from-trigger-key)
         ("C-*" . yas-insert-snippet)))


(provide 'setup)
