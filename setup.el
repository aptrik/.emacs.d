(use-package ace-jump-mode
  :bind ("s-SPC" . ace-jump-mode))


(use-package ace-window
  :bind ("s-o" . ace-window))


(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))


(use-package ahg
  :bind (:map ahg-status-mode-map
              ("<tab>" . ahg-status-diff)
              ("M-<delete>" . ahg-status-unmark-all))
  :commands ahg-status
  :config
  (add-hook 'ahg-status-mode-hook 'turn-on-truncate-lines))


(use-package all-the-icons
  )


(use-package anaconda-mode
  :diminish anaconda-mode)


(use-package ansible-doc
  :config
  (add-hook 'yaml-mode-hook #'ansible-doc-mode))


(use-package arc-mode
  :mode (("\\.egg\\'" . archive-mode)
         ("\\.\\(war\\|jar\\)\\'" . archive-mode))
  :config
  (add-hook 'archive-mode-hook 'turn-on-truncate-lines))


(use-package autorevert
  :bind ("C-c t A" . auto-revert-tail-mode)
  :diminish (auto-revert-mode . " Ⓐ")
  :config
  (setq auto-revert-verbose nil)
  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil)))


(use-package avoid
  :commands mouse-avoidance-mode
  :config
  (mouse-avoidance-mode 'none))


(use-package blacken
  )

(use-package bookmark
  :init
  (setq bookmark-save-flag 1
        bookmark-version-control t))


(use-package browse-kill-ring
  :bind ("C-c y" . browse-kill-ring)
  :commands browse-kill-ring
  :config
  (setq browse-kill-ring-quit-action 'save-and-restore))


(use-package bs
  :bind ("M-<f10>" . bs-show)
  :config
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


(use-package buffer-move
  :bind (("<C-M-S-up>"    . buf-move-up)
         ("<C-M-S-down>"  . buf-move-down)
         ("<C-M-S-left>"  . buf-move-left)
         ("<C-M-S-right>" . buf-move-right)))


(use-package calc
  :bind ("C-=" . calc)
  :config
  (setq calc-display-trail nil)
  (add-hook 'calc-mode-hook
            (lambda ()
              (local-set-key [kp-separator] 'calcDigit-start))))


(use-package calendar
  :config
  (calendar-set-date-style 'iso)

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

        holiday-swedish-holidays
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
         holiday-general-holidays
         holiday-local-holidays
         holiday-other-holidays
         holiday-christian-holidays
         holiday-solar-holidays
         holiday-swedish-holidays)))


(use-package cc-mode
  :mode (("\\.[ch]\\'" . c-mode)
         ("\\.\\(cc\\|hh\\)\\'" . c++-mode)
         ("\\.\\(i\\|swg\\)\\'" . c++-mode))
  :config
  (add-hook 'c-mode-hook 'setup--c-mode-hook)
  (add-hook 'c++-mode-hook 'setup--c-mode-hook)

  (defun gcc:get-include-directories ()
    (let ((found '())
          (inside nil))
      (dolist (s (s-lines (shell-command-to-string "gcc -xc++ -E -v -")))
        (if (s-starts-with? "End of search list." s)
            (setq inside nil))
        (if inside
            (add-to-list 'found (file-truename (s-trim s)) t))
        (if (s-starts-with? "#include <...> search starts here:" s)
            (setq inside t)))
      found))

  (use-package company-c-headers
    :config
    (setq company-c-headers-path-system (gcc:get-include-directories)))
  (add-to-list 'company-backends 'company-c-headers)

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

      (c-offsets-alist . ((access-label          . -)
                          (arglist-close         . 0)
                          (arglist-intro         . +)
                          (brace-list-open       . +)
                          (case-label            . 0)
                          (func-decl-cont        . c-lineup-java-throws)
                          (inher-cont            . c-lineup-java-inher)
                          (inline-open           . 0)
                          (innamespace           . +)
                          (knr-argdecl           . 0)
                          (knr-argdecl-intro     . 0)
                          (label                 . *)
                          (namespace-open        . [0])
                          (namespace-close       . [0])
                          (statement-block-intro . +)
                          (statement-case-open   . +)
                          (statement-cont
                           .
                           (,(when (fboundp 'c-no-indent-after-java-annotations)
                               'c-no-indent-after-java-annotations)
                            ,(when (fboundp 'c-lineup-assignments)
                               'c-lineup-assignments)
                            +))
                          (stream-op             . c-lineup-streamop)
                          (substatement-label    . 0)
                          (substatement-open     . +)
                          (topmost-intro         . 0)
                          )))
    "C/C++ programming style.")

  (defun setup--set-c-style ()
    "Set the current buffer's c-style."
    (interactive)
    (make-local-variable 'c-tab-always-indent)
    (setq c-tab-always-indent t)
    (c-add-style "c-c++-style" setup--c-style t)
    (setq c-basic-offset 4))

  (defun setup--c-mode-hook ()
    (which-function-mode 1)
    (subword-mode 1)

    (setup--set-c-style)

    (c-toggle-electric-state -1)
    (c-toggle-auto-newline -1)
    ;; (c-toggle-hungry-state 1)
    (abbrev-mode 0)

    (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
    (ggtags-mode 1)

    (eldoc-mode 1)

    (use-package helm-gtags)

    (local-set-key (kbd "C-.") 'company-complete)
    (local-set-key (kbd "C-c o o") 'ff-find-other-file)))


(use-package cmake-mode)


(use-package comint
  :bind (:map comint-mode-map
              ("<down>" . comint-next-input)
              ("<up>" . comint-previous-input)
              ("C-n" . comint-next-input)
              ("C-p" . comint-previous-input)
              ("C-r" . comint-history-isearch-backward))
  :config
  (setf comint-prompt-read-only t
        comint-history-isearch t))


(use-package command-log-mode
  :init
  (setq command-log-mode-key-binding-open-log "C-c t o"))


(use-package company
  :bind (("C-c .". company-complete))
  :bind (:map company-active-map
              ("SPC" . company-abort)
              ("TAB" . company-complete-common-or-cycle)
              ([tab] . company-complete-common-or-cycle))
  :diminish company-mode
  :defer 5
  :config
  (bind-key [remap completion-at-point] #'company-complete company-mode-map)
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)

  (setq company-backends (delete 'company-clang company-backends))
  (setq company-backends (delete 'company-xcode company-backends))
  (add-to-list 'company-backends 'company-keywords)
  (global-company-mode 1))


(use-package copy-as-format
  :bind (("C-c t w s" . copy-as-format-slack)
         ("C-c t w j" . copy-as-format-jira)))


(use-package cperl-mode
  :config
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
        cperl-invalid-face                nil))


(use-package css-mode
  :mode (("\\.css\\'" . css-mode))
  :config
  (setq cssm-indent-function 'cssm-c-style-indenter
        css-indent-level 2
        scss-compile-at-save nil)
  (add-hook 'css-mode-hook 'turn-on-rainbow-mode))


(use-package compilation-recenter-end
  :functions compilation-recenter-end-enable)


(use-package compile
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :config
  (setq-default compilation-always-kill nil
                compilation-ask-about-save t
                compilation-scroll-output 'first-error
                compilation-window-height 20
                compile-command (concat "gmake -C " default-directory " all"))

  ;;(add-hook 'compilation-mode-hook 'toggle-truncate-lines)
  (add-hook 'compilation-mode-hook 'compilation-recenter-end-enable))


(use-package diff-mode
  :bind (:map diff-mode-map
              ("w" . diff-ignore-whitespace-hunk)
              ("n" . diff-hunk-next)
              ("p" . diff-hunk-prev))
  :commands diff-mode
  :config
  (setq-default diff-switches "-uwd")

  (defun setup--diff-mode ()
    "diff-mode setup."
    (whitespace-mode 1))

  (add-hook 'diff-mode-hook 'setup--diff-mode))


(use-package dired
  :bind (:map dired-mode-map
              ("e" . ediff-dired-marked-files)
              ("M-<up>" . dired-up-directory)
              ("M-<down>" . dired-find-file))
  :commands dired-jump
  :config
  (setq dired-auto-revert-buffer t ; revert Dired buffer on revisiting
        dired-dwim-target t
        dired-listing-switches "-alhF"
        dired-ls-F-marks-symlinks t
        dired-omit-files "^\\.[^.]"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        wdired-allow-to-change-permissions t)

  (use-package dired-x)
  (use-package dired-subtree
    :config
    (setq dired-subtree-line-prefix "  "))

  (unbind-key "M-g" dired-mode-map)

  (defun setup--dired-mode ()
    (dired-omit-mode 1)
    (turn-on-truncate-lines)
    (set (make-variable-buffer-local 'font-lock-maximum-decoration) nil)
    (local-set-key (kbd "M-o") 'dired-omit-mode)
    (local-set-key (kbd "T") 'dired-do-touch)
    (local-set-key (kbd "j") 'diredp-next-line)
    (local-set-key (kbd "k") 'diredp-previous-line)
    (local-set-key (kbd "M-<right>") 'dired-find-file)
    (local-set-key (kbd "M-<left>") 'dired-up-directory)
    (local-set-key (kbd "M-<up>") 'dired-prev-dirline)
    (local-set-key (kbd "M-<down>") 'dired-next-dirline)
    (local-set-key (kbd "M-S-<right>") 'dired-subtree-insert)
    (local-set-key (kbd "M-S-<left>") 'dired-subtree-remove))

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
                 '("\\.epub\\'" "FBReader * &" "evince * &")))))


(use-package docker-images
  :commands docker-images)


(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))


(use-package dtrt-indent
  :diminish dtrt-indent-mode
  :init
  (setq dtrt-indent-verbosity 1)
  :config
  (add-hook 'prog-mode-hook 'dtrt-indent-mode))


(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g q" . dumb-jump-quick-look)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm))


(use-package ediff
  :config
  (setq-default ediff-ignore-similar-regions t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function (lambda (&optional arg)
                                      (if (> (frame-width) 150)
                                          (split-window-horizontally arg)
                                        (split-window-vertically arg)))))


(use-package edit-env
  :commands edit-env)


;; (use-package evil
;;   :config
;;   (evil-mode)


(use-package exec-path-from-shell
  :init
  ;;(setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments '("-l" "-i")
        exec-path-from-shell-check-startup-files nil)
  (setq exec-path-from-shell-variables
        '("GOPATH"
          "LANG"
          "MANPATH"
          "PATH"
          "PGPPATH"
          "PYTHONPATH"
          "SSH_AGENT_PID" "SSH_AUTH_SOCK"))
  :config
  ;;(when (memq window-system '(mac ns))
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))


(use-package eyebrowse
  :config
  (eyebrowse-mode t))


(use-package expand-region
  :bind (("C-+" . er/expand-region)
         ("C-?" . er/contract-region)))


(use-package eww
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="
        eww-download-directory "~/dl/"))


(use-package find-dired
  :bind (("M-s D" . find-dired)
         ("M-s d" . find-grep-dired)
         ("M-s n" . find-name-dired)))


(use-package fixme)


(use-package flycheck
  :bind ("C-c t f" . flycheck-mode)
  :config
  (setq flycheck-pylint-use-symbolic-id nil)

  (flycheck-add-next-checker 'python-flake8 '(t . python-pylint))

  (flycheck-define-checker proselint
                           "A linter for prose."
                           :command ("proselint" source-inplace)
                           :error-patterns
                           ((warning line-start (file-name) ":" line ":" column ": "
                                     (id (one-or-more (not (any " "))))
                                     (message) line-end))
                           :modes (text-mode markdown-mode gfm-mode))
  (add-to-list 'flycheck-checkers 'proselint)

  (use-package flycheck-color-mode-line
    :config
    (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))


(use-package focus-autosave-mode
  :diminish focus-autosave-mode
  :init
  (focus-autosave-mode 1))


(use-package framemove
  :init
  (windmove-default-keybindings 'shift)
  ;; Cannot wrap and have framemove do its thing at the same time.
  (setq windmove-wrap-around nil
        framemove-hook-into-windmove t))


(use-package ggtags
  :commands ggtags-mode
  :diminish ggtags-mode)


(use-package gitconfig-mode
  :mode ("/gitconfig\\'" . gitconfig-mode))


(use-package gitignore-mode
  :mode ("/gitignore\\'" . gitignore-mode))


(use-package go-mode
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("C-c C-r" . go-remove-unused-imports)
              ("C-c C-g" . go-goto-imports))
  :config
  ;;(setq gofmt-command "goimports")
  (use-package golint)
  (use-package go-eldoc)
  (use-package go-guru)

  (defun setup--go-mode ()
    (setq tab-width 4)
    (company-mode 1)
    (flycheck-mode 1)
    (go-eldoc-setup)
    (subword-mode 1)
    (which-function-mode 1)

    ;;(idle-highlight-mode 1)
    (go-guru-hl-identifier-mode 1)

    (set (make-local-variable 'compile-command)
         "go build -v && go test -v && go vet"))

  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'setup--go-mode))


(use-package gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))


(use-package grep
  :bind (("M-s f" . find-grep)
         ("M-s g" . grep)
         ("M-s r" . rgrep))
  :init
  (setq grep-files-aliases
        '(("el" . "*.el")
          ("c"  . "*.c")
          ("h"  . "*.h")
          ("cc" . "*.hh *.hpp *.cc *.cpp")
          ("hh" . "*.hh *.hpp *.cc *.cpp")))
  :config
  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52))

  (add-hook 'grep-mode-hook 'turn-on-truncate-lines)

  (add-to-list 'grep-find-ignored-directories "elpa")
  (add-to-list 'grep-find-ignored-directories "target")
  (add-to-list 'grep-find-ignored-directories "vendor")

  (use-package grep-a-lot
    :config
    (grep-a-lot-setup-keys))

  (let ((find-command "find . \\( -path '*/CVS' -o -path '*/.hg' -o -path '*/.git' \\) -prune -o -type f -print0"))
    (if macosp
        (setq grep-find-command
              (concat find-command " | xargs -0 " grep-command))
      (setq grep-find-command
            (concat find-command " | xargs -0 -e " grep-command)))))


(use-package groovy-mode
  :mode (("\\.grovvy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))


(use-package gud
  :config
  (setq-default gdb-many-windows t
                gdb-use-separate-io-buffer t
                gud-tooltip-mode t))


(use-package helm
  :config
  (helm-autoresize-mode 1))


(use-package helm-projectile
  :bind (("C-x f" . helm-projectile-find-file)
         ("C-c o p" . helm-projectile-find-file))
  :config
  (helm-projectile-on))


(use-package highlight-symbol
  :config
  (add-hook 'prog-mode-hook #'highlight-symbol-nav-mode))


(use-package hl-line
  :config
  (global-hl-line-mode 1))


(use-package hl-tags-mode
  :commands hl-tags-mode)


(use-package hl-todo
  :init
  (global-hl-todo-mode))


(use-package html5-schema)


(use-package ibuffer
  :bind (([remap list-buffers] . ibuffer))
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))))


(use-package ibuffer-vc
  :init
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))


(use-package idle-highlight-mode
  :bind ("C-c t i" . idle-highlight-mode))


(use-package iedit
  :bind ("M-RET" . iedit-mode))


(use-package ispell
  :bind (("C-c s b" . ispell-buffer)
         ("C-c s c" . ispell-comments-and-strings)
         ("C-c s d" . ispell-change-dictionary)
         ("C-c s r" . ispell-region)
         ("C-c s w" . ispell-word)
         ("C-c <f11>" . cycle-ispell-languages))
  :init
  (setq ispell-dictionary "en_GB"
        ispell-help-in-bufferp 'electric
        ispell-program-name (executable-find "hunspell")
        ispell-silently-savep t)
  :config
  (let ((langs '("english" "svenska")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs)
      (ring-insert lang-ring elem)))

  (defun cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang))))


(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))
  :commands (js2-print-json-path)
  :config
  (setq-default
   js2-global-externs
   '("module" "require" "assert" "refute"
     "setTimeout" "clearTimeout" "setInterval" "clearInterval"
     "location" "__dirname" "console" "JSON")
   js2-additional-externs
   '("$" "unsafeWindow" "localStorage" "jQuery"
     "setTimeout" "setInterval" "location" "console")
   js2-strict-missing-semi-warning t
   js2-strict-trailing-comma-warning t)
  (setq
   js2-basic-offset 4
   js2-highlight-level 3
   js2-indent-switch-body t
   js2-skip-preprocessor-directives t
   js2-use-font-lock-faces t)

  (use-package js2-highlight-vars)
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :config
    (js2r-add-keybindings-with-prefix "C-c C-m"))
  (use-package karma)
  (use-package nodejs-repl)

  (use-package tern)
  (use-package company-tern)
  (add-to-list 'company-backends 'company-tern)

  (use-package web-beautify
    :commands (web-beautify-js web-beautify-js-buffer))

  (use-package ac-js2)
  (add-to-list 'company-backends 'ac-js2-company)

  (defun delete-tern-process ()
    (interactive)
    (delete-process "Tern"))

  (defun setup--js2-mode ()
    (subword-mode 1)
    (flycheck-mode 1)
    (company-mode 1)
    (js2-refactor-mode 1)
    (karma-mode 1)
    ;;(js2-highlight-vars-mode 1)

    (local-set-key (kbd "C-.") 'company-complete)
    (local-set-key (kbd "C-x C-e") 'nodejs-repl-send-last-sexp)
    (local-set-key (kbd "M-q") 'web-beautify-js)

    (local-unset-key (kbd "C-c e"))
    (local-set-key (kbd "C-c e e") 'nodejs-repl-send-last-sexp)
    (local-set-key (kbd "C-c e n") 'nodejs-repl)
    (local-set-key (kbd "C-c e p") 'js2-print-json-path)
    (local-set-key (kbd "C-c e q") 'nodejs-repl-quit-or-cancel)
    (local-set-key (kbd "C-c e r") 'nodejs-repl-send-region)
    (local-set-key (kbd "C-c e v") 'nodejs-repl-switch-to-repl))

  (add-hook 'js2-mode-hook 'setup--js2-mode))


(use-package json-mode
  :preface
  (defun json-mode--reformat-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'json-reformat-region)
      (json-reformat-region (point-min) (point-max))))
  :bind (:map json-mode-map
              ("M-q" . json-mode--reformat-region))
  :config
  (setf json-reformat:pretty-string? t
        json-reformat:indent-width 2))


(use-package keyfreq
  :config
  (setq keyfreq-excluded-commands
        '(backward-char beginning-of-line end-of-line forward-char newline next-line previous-line self-insert-command left-char right-char))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package lisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-<f9>" . ert-run-tests-interactively)
              ("M-&" . lisp-complete-symbol)
              ("C-<delete>" . sp-kill-sexp)
              ("C-<backspace>" . sp-backward-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-M-t" . sp-transpose-sexp)
              ("M-q" . sp-indent-defun)
              ("C-c e E" . toggle-debug-on-error)
              ("C-c e e" . eval-last-sexp)
              ("C-c e m" . macrostep-expand)
              ("C-c e r" . eval-region))
  :mode ("Cask" . emacs-lisp-mode)
  :config
  (use-package eldoc
    :diminish eldoc-mode
    :init
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
  (use-package elisp-slime-nav
    :diminish elisp-slime-nav-mode
    :init
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode))
  (use-package elint
    :bind ("C-c e E" . elint-current-buffer)
    :commands 'elint-initialize
    :preface
    (defun elint-current-buffer ()
      (interactive)
      (elint-initialize)
      (elint-current-buffer))
    :config
    (add-to-list 'elint-standard-variables 'current-prefix-arg)
    (add-to-list 'elint-standard-variables 'command-line-args-left)
    (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
    (add-to-list 'elint-standard-variables 'emacs-major-version)
    (add-to-list 'elint-standard-variables 'window-system))
  (use-package ert
    :bind ("C-c e t" . ert-run-tests-interactively)
    :commands ert-run-tests-interactively
    :config
    (put 'ert-deftest 'lisp-indent-function 'defun)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (font-lock-add-keywords
                 nil
                 '(("(\\(\\<ert-deftest\\)\\>\\s *\\(\\sw+\\)?"
                    (1 font-lock-keyword-face nil t)
                    (2 font-lock-function-name-face nil t)))))))

  (defun setup--emacs-lisp-mode ()
    (add-hook 'after-save-hook 'check-parens nil t)
    (company-mode 1)
    (local-set-key (kbd "C-.") 'company-complete))

  (add-hook 'emacs-lisp-mode-hook 'setup--emacs-lisp-mode))


(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))


(use-package magit
  :init
  (autoload 'if-let "subr-x")
  :config
  (setq ;; magit-completing-read-function 'ivy-completing-read
   magit-diff-refine-hunk t
   magit-push-always-verify nil
   magit-repository-directories '("~/.emacs.d")
   magit-restore-window-configuration t
   magit-status-buffer-switch-function 'switch-to-buffer)
  (global-magit-file-mode 1)
  (magit-auto-revert-mode 1)
  (use-package magit-blame)
  (defadvice magit-diff-working-tree (after magit-diff-focus activate)
    "After execution, select the magit-diff buffer in the current window."
    (other-window 1))
  (fullframe magit-status magit-mode-quite-window))


(use-package man
  :config
  (setq Man-notify-method 'pushy))


(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command "pandoc"))


(use-package minimap
  :init
  (setq minimap-active-region-background nil
        minimap-window-location 'right))


(use-package misc
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char))
  :commands zap-up-to-char)


(use-package move-text
  :bind (("<C-S-up>" . move-text-up)
         ("<C-S-down>" . move-text-down)))


(use-package multi-line
  :bind ("s-," . multi-line))


(use-package multiple-cursors
  :defer 5
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
         ("C-c o a"     . mc/vertical-align)
         ("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o k"     . mc/mark-previous-like-this)
         ("C-c o j"     . mc/mark-next-like-this)
         ("C-c o r"     . mc/mc-mark)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o C-s"   . mc/mark-all-in-region)
         ("s-<mouse-1>" . mc/add-cursor-on-click)
         ("C-|"         . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)))


(use-package neotree
  :bind ("s-q" . neotree-toggle)
  :config
  (setq neo-auto-indent-point t
        neo-show-hidden-files nil
        neo-modern-sidebar t
        neo-smart-open t
        neo-theme 'icons
        neo-window-width 35))


(use-package newcomment
  :bind ("C-;" . comment-line))


(use-package nlinum
  :bind ("C-c t l" . nlinum-mode))


(use-package nuke-whitespace
  :bind ("C-c t n" . nuke-trailing-whitespace))


(use-package nxml-mode
  :config
  (setq nxml-auto-insert-xml-declaration-flag nil
        nxml-bind-meta-tab-to-complete-flag t
        nxml-child-indent 2
        nxml-slash-auto-complete-flag t
        nxml-syntax-highlight-flag t
        rng-nxml-auto-validate-flag nil)
  ;; (setq nxml-child-indent 4 nxml-attribute-indent 4)
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (add-hook 'sgml-mode-hook 'turn-on-hl-tags-mode)
  (add-hook 'nxml-mode-hook 'turn-on-hl-tags-mode))


(use-package openwith
  :config
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
         ;; (list (openwith-make-extension-regexp
         ;;        '("pdf")) ; "ps" "ps.gz" "dvi"))
         ;;       (if macosp "open" "evince")
         ;;       '(file))
         (list (openwith-make-extension-regexp
                '("epub"))
               (if macosp "open" "FBReader")
               '(file))))
  (openwith-mode 1))


(use-package org
  :bind (("M-m" . org-capture)
         ("C-c a" . org-agenda))
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-replace-disputed-keys t
        org-export-backends '(ascii html md reveal twbs))
  :config
  (setq
   org-clock-history-length 20
   org-clock-in-resume t
   org-hide-leading-stars nil
   org-level-color-stars-only t
   org-log-done 'time
   org-odd-levels-only nil
   org-reverse-note-order t
   org-src-fontify-natively t
   org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   org-time-stamp-rounding-minutes '(0 5)
   org-use-speed-commands t)

  (setq org-agenda-files '())

  ;; (setq org-clock-persist t
  ;;       org-clock-persist-query-resume nil)
  ;; (org-clock-persistence-insinuate)

  (add-hook 'org-mode-hook 'setup--org-mode)

  (defun setup--org-mode ()
    (setq org-blank-before-new-entry '((heading . t)
                                       (plain-list-item . nil)))

    (require 'ox-latex)

    ;; Set program to use when opening PDF files.
    (if macosp
        (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
      (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))

    ;; Enable languages for in-buffer evaluation.
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (ruby . t)
       (shell . t)))

    (local-set-key [M-up]   'outline-previous-visible-heading)
    (local-set-key [M-down] 'outline-next-visible-heading)))


(use-package paren
  :init
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))


(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (setq-default pdf-view-display-size 'fit-width)
    (setq pdf-annot-activate-created-annotations t
          pdf-view-resize-factor 1.10)
    (pdf-tools-install)))


(use-package php-mode
  :config
  (setq php-extra-constants '())
  (add-hook 'php-mode-hook 'setup--php-mode)

  (defun setup--php-mode ()
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
                       " \"" (buffer-file-name) "\"")))))


(lexical-let
    ((jar-file (expand-file-name "/usr/share/java/plantuml.jar")))
  (if (file-exists-p jar-file)
      (use-package plantuml-mode
        :config
        (setq plantuml-jar-path jar-file))))


(use-package popup-imenu
  :bind (("s-i" . popup-imenu)
         :map popup-isearch-keymap
         ("s-i" . popup-isearch-cancel))
  :config
  (setq popup-imenu-position 'point
        popup-imenu-style 'indent))


(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended))


(use-package printing
  :config
  (pr-update-menus t)
  (setq lpr-command          "lpr"
        lpr-headers-switches "-h"
        ps-paper-type         'a4
        ps-print-color-p      nil
        ps-number-of-columns  2
        ps-landscape-mode     t))


(use-package projectile
  :commands (projectile-mode projectile-global-mode)
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'helm
        projectile-create-missing-test-files t
        projectile-enable-caching t
        projectile-ignored-project-function #'file-remote-p
        projectile-indexing-method 'native
        projectile-require-project-root t
        projectile-track-known-projects-automatically t)
  :config
  (add-to-list 'projectile-globally-ignored-directories "target")
  (add-to-list 'projectile-globally-ignored-files ".DS_Store")
  (projectile-global-mode 1))


(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))


(use-package python
  :bind (:map python-mode-map
              ("C-." . company-complete)
              ("C-c I" . py-isort-buffer)
              ("C-c 2 3" . python-2to3-current-buffer)
              ("<f9>" . py-run)
              ("<S-f9>" . pdb)
              ("<C-f9>" . compile)
              ("<M-f9>" . recompile))
  :init
  (add-hook 'python-mode-hook 'setup--python-mode)

  ;; (setenv "PYTHONPATH" (concat (if (getenv "PYTHONPATH") "$PYTHONPATH:" "")
  ;;                              (expand-file-name "bin/lib/python" user-emacs-directory))
  ;;         t)

  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive
     (list (gud-query-cmdline 'pdb
                              (file-name-nondirectory buffer-file-name)))))

  (defun py-run ()
    "Run python on the file in the current buffer."
    (interactive)
    (compile (format "python \"%s\"" (buffer-file-name))))

  (use-package company-anaconda)
  (add-to-list 'company-backends 'company-anaconda)

  ;; (use-package importmagic
  ;;   :config
  ;;   (add-hook 'python-mode-hook 'importmagic-mode))

  (defun setup--python-mode ()
    ;;(setq py-python-command-args '( "-colors" "Linux"))

    (modify-syntax-entry ?\_ "_" python-mode-syntax-table)

    (subword-mode 1)
    (which-function-mode 1)

    (anaconda-mode 1)
    (eldoc-mode 1)
    (turn-on-anaconda-eldoc-mode)
    (company-mode 1)
    (flycheck-mode 1)
    (idle-highlight-mode 1)

    (require 'sphinx-doc)
    (sphinx-doc-mode 1)

    (set (make-variable-buffer-local 'outline-regexp) "def\\|class ")
    (set (make-variable-buffer-local 'indent-tabs-mode) nil))
  :config
  (use-package py-isort
    :commands (py-isort-buffer))
  )


(use-package rbenv
  :init
  (setq rbenv-show-active-ruby-in-modeline nil))


(use-package re-builder
  :config
  (setq reb-re-syntax 'string))


(use-package rst
  :config
  (add-hook 'rst-mode-hook 'setup--rst-mode)

  (defun setup--rst-mode ()
    (sphinx-mode 1)))


(use-package ruby-mode
  :mode (("Gemfile\\'" . ruby-mode)
         ("Rakefile\\'" . ruby-mode)
         ("Guardfile" . ruby-mode)
         ("Vagrantfile\\'" . ruby-mode)
         ("\\.watchr\\'" . ruby-mode)
         ("\\.rake\\'" . ruby-mode)
         ("\\.rb\\'" . ruby-mode))
  :init
  (setq ruby-deep-indent-paren nil)

  (add-hook 'ruby-mode-hook 'setup--ruby-mode)

  (defun ruby-run ()
    "Run ruby on the file in the current buffer."
    (interactive)
    (compile (concat "ruby " (buffer-file-name))))

  (defun setup--ruby-mode ()
    (which-function-mode 1)
    (subword-mode 1)
    (company-mode 1)
    (robe-mode 1)
    (inf-ruby-minor-mode 1)
    (inf-ruby-switch-setup)

    (minitest-mode 1)

    (make-variable-buffer-local 'compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist
          (append compilation-error-regexp-alist
                  (list (list
                         (concat
                          "\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\)"
                          ":\\([0-9]+\\)") 2 3))))

    (make-variable-buffer-local 'compile-command)
    (setq compile-command (concat "ruby " (buffer-file-name) " "))

    (local-set-key (kbd "C-.") 'company-complete)

    (local-set-key [f9]    'ruby-run)
    (local-set-key [C-f9]  'minitest-verify)
    (local-set-key [M-f9]  'minitest-verify-single))
  :config
  (use-package ruby-end
    :diminish ruby-end-mode)
  (use-package robe
    :diminish robe-mode))


(use-package sh-script
  :mode (("/\\.\\(my\\)?login\\'" . sh-mode)
         ("/\\.\\(my\\)?logout\\'" . sh-mode)
         ("/\\.\\(my\\)?t?cshrc\\'" . sh-mode)
         ("/\\.profile\\'" . sh-mode)
         ("/\\.xinitrc\\'" . sh-mode)
         ("\\.t?c?sh\\'" . sh-mode))
  :init
  (setq sh-shell-file     "/bin/sh"
        sh-indentation    4
        sh-basic-offset   4
        sh-indent-comment t))


(use-package shackle
  ;; https://github.com/wasamasa/shackle
  :config
  (setq helm-display-function 'pop-to-buffer) ; make helm play nice
  (setq shackle-default-alignment 'below
        ;; shackle-default-rule '(:select t)
        shackle-default-size 0.25)
  (setq shackle-rules
        '(("*Man.*"                 :same t :regexp t)
          ("*Shell Command Output*" :noselect t :size 0.5)
          ("*helm.*"                :select t :size 0.33 :regexp t)
          (aHg-status-mode          :same t)
          (compilation-mode         :noselect t)
          (flycheck-error-list-mode :noselect t)
          (help-mode                :select t :size 0.5)
          (helpful-mode             :select t :size 0.5)
          (magit-diff-mode          :noselect t)
          ))
  (shackle-mode t))


(use-package shell
  :init
  (setq explicit-shell-file-name "bash"
        shell-file-name shell-file-name
        shell-command-switch "-c")
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


(use-package shell-toggle
  :bind ("s-t" . shell-toggle))


(use-package savehist
  :init
  (setq savehist-ignored-variables '(file-name-history))
  (savehist-mode 1))


(use-package saveplace
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  :config
  (save-place-mode t))


(use-package scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :config
  (add-hook 'scss-mode-hook 'turn-on-rainbow-mode))


(use-package shrink-whitespace
  :bind ("M-SPC" . shrink-whitespace))


(use-package smartparens
  :commands (smartparens-mode
             smartparens-strict-mode
             show-smartparens-mode
             sp-kill-sexp sp-backward-kill-sexp
             sp-copy-sexp
             sp-forward-slurp-sexp
             sp-forward-barf-sexp
             sp-backward-slurp-sexp
             sp-backward-barf-sexp
             sp-transpose-sexp
             sp-indent-defun)
  :config
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
     ("M-q" . sp-indent-defun))))


(use-package smooth-scrolling)


(use-package solarized-theme
  :init
  (load-theme 'solarized-dark t)
  :config
  (setf frame-background-mode 'dark)

  (custom-set-faces
   '(cursor ((t (:foreground "white" :background "firebrick" :inverse-video t))))
   ;; '(region ((t (:foreground "black" :background "#b5d5ff"))))
   '(match ((t (:background "#FFFF66" :foreground "black" :weight bold))))
   '(highlight ((t :background "black" :underline nil)))

   ;; diff
   '(diff-added ((t (:foreground "green4" :underline nil))))
   '(diff-changed ((t (:foreground "blue3"))))
   '(diff-removed ((t (:foreground "red4" :underline nil))))
   '(diff-refine-added ((t (:background "#446644" :foreground "green"))))
   '(diff-refine-changed ((t (:background "#ddddff" :foreground "blue"))))
   '(diff-refine-removed ((t (:background "#664444" :foreground "red"))))

   ;; magit
   '(magit-item-highlight ((t nil)))

   ;; hl-line-mode
   '(hl-line-face ((t (:background "gray10"))))

   ;; idle-highlight
   '(idle-highlight ((t (:foreground "yellow" :background "black"))))

   ;; isearch
   '(isearch
     ((t (:foreground "black" :background "yellow" :bold t))))
   '(isearch-lazy-highlight-face
     ((t (:foreground "black" :background "yellow"))))
   '(isearch-fail
     ((t (:foreground "red" :background "yellow" :bold t))))

   ;; show-paren
   '(show-paren-match
     ((t (:foreground "black" :background "dark green" :bold t))))
   '(show-paren-mismatch
     ((t (:foreground "yellow" :background "indian red" :bold t))))
   ))


(use-package spacemacs-theme
  :disabled
  :init
  (load-theme 'spacemacs-dark t)
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil)
  :config
  (custom-set-faces
   ;; idle-highlight
   '(idle-highlight ((t (:foreground "yellow" :background "black"))))

   ;; isearch
   '(isearch
     ((t (:foreground "black" :background "yellow" :bold t))))
   '(isearch-lazy-highlight-face
     ((t (:foreground "black" :background "#ffff99")))) ; Canary Yellow
   '(isearch-fail
     ((t (:foreground "red" :background "yellow" :bold t))))))


(use-package spaceline
  :disabled
  :demand t
  :init
  (setq powerline-default-separator 'arrow-fade)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme))


(use-package speedbar
  :config
  (setq speedbar-default-position 'left
        speedbar-show-unknown-files t
        speedbar-update-flag t))


(use-package sphinx-doc
  :diminish sphinx-doc-mode)


(use-package sql
  :config
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
    (sql-rename-buffer)))


;; (use-package stripe-buffer
;;   :init
;;   (add-hook 'dired-mode-hook #'stripe-buffer-mode))


(use-package subword
  :bind (:map subword-mode-map
              ("<M-left>"      . subword-backward)
              ("<M-right>"     . subword-forward)
              ("<C-left>"      . subword-backward)
              ("<C-right>"     . subword-forward)
              ("<C-backspace>" . subword-backward-kill))
  :diminish subword-mode)


(use-package term
  :config
  (defadvice ansi-term (before force-bash)
    (interactive (list "/bin/bash")))
  (defadvice term (before force-bash)
    (interactive (list "/bin/bash")))
  (ad-activate 'ansi-term)
  (ad-activate 'term)

  (defun setup--term-paste (&optional string)
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (if string string (current-kill 0))))

  (defun setup--term ()
    (goto-address-mode))
  ;; (define-key term-raw-map (kbd "M-o") 'other-window)
  ;; (define-key term-raw-map (kbd "M-p") 'term-send-up)
  ;; (define-key term-raw-map (kbd "M-n") 'term-send-down)
  ;; (define-key term-raw-map (kbd "C-y") 'setup--term-paste))

  (add-hook 'term-mode-hook 'setup--term))


(use-package terraform-mode
  :mode "\\.tf\\'"
  :config
  (use-package company-terraform)
  (company-terraform-init)
  ;; (add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.json.tftemplate\\'" . json-mode))
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))


(use-package time
  :config
  (setq display-time-world-time-format "%Y-%m-%d %H:%M %Z"
        display-time-world-list '(("America/Los_Angeles" "San Fransisco")
                                  ("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Stockholm" "Stockholm")
                                  ("Asia/Tokyo" "Tokyo"))))


(use-package tramp
  :init
  (setq tramp-default-method 'ssh))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package vc
  :config
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
  (define-key vc-prefix-map "R" 'vc-resolve-conflicts)

  (fullframe vc-dir quit-window)

  (defun setup--cvs-mode ()
    (local-set-key [M-delete] 'cvs-mode-unmark-all-files)
    (local-set-key [return]   'cvs-mode-find-file))

  (add-hook 'cvs-mode-hook 'setup--cvs-mode))


(use-package vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))


(use-package vlf-setup)


(use-package web-mode
  :mode (("\\.html\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.\\(php\\|inc\\)\\'" . web-mode))
  :config
  (setq web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-disable-css-colorization nil
        web-mode-disable-auto-pairing nil))


(use-package webjump
  :config
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
                     "&words=") ""]))))


(use-package wgrep)


(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))


(use-package ws-butler
  :diminish ws-butler-mode
  :config
  (setq ws-butler-keep-whitespace-before-point nil)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))


(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))


(use-package yasnippet
  :bind (("C-c t y" . company-yasnippet))
  :commands (snippet-mode yas-expand yas-minor-mode)
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (setq yas-verbosity 0)
  (yas-global-mode 1)
  (setq-default yas-prompt-functions
                '(yas/ido-prompt yas/completing-prompt))
  :config
  (load "snippet-helpers"))


(use-package yasnippet-snippets
  :ensure
  :after yasnippet
  :config (yasnippet-snippets-initialize))


(use-package zeal-at-point
  :bind ("C-c t z" . zeal-at-point)
  :config
  (setq zeal-at-point-mode-alist
        (delete
         (assoc 'python-mode zeal-at-point-mode-alist)
         zeal-at-point-mode-alist))
  (add-to-list 'zeal-at-point-mode-alist
               '(python-mode . "python")))


(provide 'setup)
