;; -*- lexical-binding: t -*-

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))


(use-package aggressive-indent
  :disabled
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))


(use-package ahg
  :disabled t
  :bind (:map ahg-status-mode-map
              ("<tab>" . ahg-status-diff)
              ("M-<delete>" . ahg-status-unmark-all))
  :commands ahg-status
  :hook (ahg-status-mode . turn-on-truncate-lines))


(use-package ansible-doc
  :hook (yaml-mode . ansible-doc-mode))


(use-package arc-mode
  :mode (("\\.egg\\'" . archive-mode)
         ("\\.\\(war\\|jar\\)\\'" . archive-mode))
  :hook (archive-mode . turn-on-truncate-lines))


(use-package autorevert
  :defer t
  :bind ("C-c t A" . auto-revert-tail-mode)
  :diminish (auto-revert-mode . " Ⓐ")
  :init
  (setq auto-revert-verbose nil)
  (when (eq system-type 'darwin)
    ;; File notifications aren't supported on OS X
    (setq auto-revert-use-notify nil)))


(use-package blacken
  :after python-mode
  :commands blacken-mode)


(use-package bookmark
  :defer t
  :commands (bookmark-bmenu-list bookmark-jump bookmark-set)
  :config
  (setq bookmark-save-flag 1
        bookmark-version-control t))


(use-package browse-kill-ring
  :bind ("C-c y" . browse-kill-ring)
  :commands browse-kill-ring
  :config
  (setq browse-kill-ring-quit-action 'save-and-restore))


(use-package bs
  :commands bs-show
  :bind (("M-<f10>" . bs-show)
         ("C-c <f10>" . bs-show))
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
  :defer t
  :commands calc
  :bind (:map calc-mode-map
              ([kp-separator] . calcDigit-start))
  :config
  (setq calc-display-trail nil))
  ;; :hook (calc-mode . (lambda ()
  ;;                      (local-set-key [kp-separator] 'calcDigit-start))))


(use-package calendar
  :defer t
  :commands calendar
  :hook ((diary-display . fancy-diary-display)
         (today-visible-calendar . calendar-mark-today)
         (list-diary-entries . sort-diary-entries))
  :config
  (calendar-set-date-style 'iso)

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


(use-package comint
  :ensure nil
  :defer t
  :bind (:map comint-mode-map
              ("<down>" . comint-next-input)
              ("<up>" . comint-previous-input)
              ("C-n" . comint-next-input)
              ("C-p" . comint-previous-input)
              ("C-r" . comint-history-isearch-backward))
  :config
  (setf comint-prompt-read-only t
        comint-history-isearch t))


(use-package company
  :defer t
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :bind (("C-c .". company-complete))
  :bind (:map company-active-map
              ("SPC" . company-abort)
              ("TAB" . company-complete-common-or-cycle)
              ([tab] . company-complete-common-or-cycle))
  :hook (after-init . global-company-mode)
  :init
  (dolist (hook '(emacs-lisp-mode-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  (setq company-backends '((company-capf company-files company-keywords company-dabbrev-code))
        company-begin-commands '(self-insert-command)
        company-echo-delay 0
        company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-show-numbers t
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-tooltip-limit 20))


(use-package company-box
  :defer t
  :after company
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))


(use-package company-elisp
  :ensure nil
  :after company
  :config
  (push 'company-elisp company-backends)
  (setq-local company-backend '(company-elisp)))


(use-package company-go
  :defer t)


(use-package company-terraform
  :after (company terraform-mode))


(use-package copy-as-format
  :bind (("C-c t w g" . copy-as-format-github)
         ("C-c t w j" . copy-as-format-jira)
         ("C-c t w m" . copy-as-format-markdown)
         ("C-c t w o" . copy-as-format-org-mode)
         ("C-c t w r" . copy-as-format-rst)
         ("C-c t w s" . copy-as-format-slack)))


(use-package cperl-mode
  :disabled t
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
  :defer t
  :mode (("\\.css\\'" . css-mode))
  :hook (css-mode . turn-on-rainbow-mode)
  :config
  (setq cssm-indent-function 'cssm-c-style-indenter
        css-indent-level 2
        scss-compile-at-save nil))


(use-package compilation-recenter-end
  :ensure nil
  :functions compilation-recenter-end-enable)


(use-package compile
  :no-require
  :bind (("C-c c" . compile)
         ("C-c C" . recompile))
  :hook (;;(compilation-mode . toggle-truncate-lines)
         (compilation-mode . compilation-recenter-end-enable)
         (compilation-filter . (lambda () (ansi-color-apply-on-region (point-min) (point-max)))))
  :config
  (setq-default compilation-always-kill nil
                compilation-ask-about-save t
                compilation-scroll-output 'first-error
                compilation-window-height 20
                compile-command (concat "gmake -C " default-directory " all")))


(use-package diff-mode
  :defer t
  :bind (:map diff-mode-map
              ("w" . diff-ignore-whitespace-hunk)
              ("n" . diff-hunk-next)
              ("p" . diff-hunk-prev))
  :commands diff-mode
  :hook (diff-mode . whitespace-mode)
  :config
  (setq-default diff-switches "-uwd"))


(use-package dired
  :ensure nil
  :defer t
  :bind ("C-c j" . dired--downloads)
  :bind (:map dired-mode-map
              ("<tab>" . dired-next-window)
              ("C-<down>" . dired-next-dirline)
              ("C-<up>" . dired-prev-dirline)
              ("M-<up>" . dired-up-directory)
              ("M-<down>" . dired-find-file)
              ("M-o" . dired-omit-mode)
              ("T" . dired-do-touch)
              ("e" . ediff-dired-marked-files))
  :commands dired-jump
  :hook ((dired-mode . dired-omit-mode)
         (dired-mode . turn-on-truncate-lines))
  :init
  (defun dired--downloads ()
    (interactive)
    (push-window-configuration)
    (let ((here default-directory))
      (delete-other-windows)
      (dired "~/dl")
      (split-window-horizontally)
      (dired here)))
  (defun dired-next-window ()
    (interactive)
    (let ((next (car (cl-remove-if-not #'(lambda (wind)
                                           (with-current-buffer (window-buffer wind)
                                             (eq major-mode 'dired-mode)))
                                       (cdr (window-list))))))
      (when next
        (select-window next))))
  :config
  (setq dired-auto-revert-buffer t ; revert Dired buffer on revisiting
        dired-dwim-target t
        dired-listing-switches "-alhFG"
        dired-ls-F-marks-symlinks t
        dired-omit-files "^\\.[^.]"
        dired-recursive-copies 'always
        dired-recursive-deletes 'always
        wdired-allow-to-change-permissions t)
  (unbind-key "M-g" dired-mode-map)
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

(use-package dired-x
  :ensure nil
  :after dired)


(use-package dired-subtree
  :after dired
  :config
  (setq dired-subtree-line-prefix "  "))


(use-package direnv
  :defer t
  :config
  (direnv-mode))


(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))


(use-package dtrt-indent
  :defer t
  :hook (prog-mode . dtrt-indent-mode)
  :diminish dtrt-indent-mode
  :init
  (setq dtrt-indent-verbosity 1))


(use-package dumb-jump
  :init
  (setq dumb-jump-force-searcher 'rg
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  :hook (xref-backend-functions . dumb-jump-xref-activate))


(use-package ediff
  :defer t
  :commands (ediff-files ediff-buffers)
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = B" . ediff-buffers3)
         ("C-c = c" . compare-windows)
         ("C-c = =" . ediff-files)
         ("C-c = f" . ediff-files)
         ("C-c = F" . ediff-files3)
         ("C-c = r" . ediff-revision)
         ("C-c = p" . ediff-patch-file)
         ("C-c = P" . ediff-patch-buffer)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise))
  :config
  (setq-default ediff-ignore-similar-regions t)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function (lambda (&optional arg)
                                      (if (> (frame-width) 150)
                                          (split-window-horizontally arg)
                                        (split-window-vertically arg)))))


(use-package edit-env
  :ensure nil
  :commands edit-env)


(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :commands eldoc-mode
  :hook (emacs-lisp-mode . eldoc-mode)
  :init
  (setq eldoc-echo-area-use-multiline-p nil
        eldoc-idle-delay 0.5
        eldoc-print-after-edit nil))


(use-package elint
  :commands elint-initialize
  :bind ("C-c e E" . elint-current-buffer)
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


(use-package elisp-docstring-mode
  :commands elisp-docstring-mode)


(use-package elisp-slime-nav
  :diminish
  :commands (elisp-slime-nav-mode elisp-slime-nav-find-elisp-thing-at-point))


(use-package elmacro
  :bind (("C-c m e" . elmacro-mode)
         ("C-x C-)" . elmacro-show-last-macro)))


(use-package ert
  :bind (("C-c e t" . ert-run-tests-interactively)))


(use-package exec-path-from-shell
  :if (display-graphic-p)
  :init
  ;;(setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments '("-l" "-i")
        exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables
        '("DISPLAY"
          "SSH_AGENT_PID"
          "SSH_ASKPASS"
          "SSH_AUTH_SOCK"
          "SSH_CONNECTION"
          "TMUX_PROJECT_PATH"
          "WINDOWID"
          "XAUTHORITY"
          "LANG"
          "MANPATH"
          "PATH"
          "PGPPATH"
          "PYTHONPATH"
          "SSH_AGENT_PID"
          ))
  :config
  (exec-path-from-shell-initialize))


(use-package expand-region
  :bind (("C-+" . er/expand-region)
         ("C-?" . er/contract-region)
         ("<M-S-left>" . er/contract-region)
         ("<M-S-right>" . er/expand-region)
         ))


(use-package eyebrowse
  :disabled t
  :config
  (eyebrowse-mode t))


(use-package find-dired
  :bind (("M-s D" . find-dired)
         ("M-s d" . find-grep-dired)
         ("M-s n" . find-name-dired)))


(use-package flycheck
  :defer t
  :commands (flycheck-mode
             flycheck-next-error
             flycheck-previous-error)
  :bind ("C-c t f" . flycheck-mode)
  :config
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get))


(use-package flycheck-color-mode-line
  :defer t
  :after flycheck
  :commands flycheck-color-mode-line-mode
  :config
  :hook (flycheck-mode . flycheck-color-mode-line-mode))


(use-package flycheck-golangci-lint
  :commands flycheck-golangci-lint-setup
  :config
  (setq flycheck-golangci-lint-fast t))


(use-package flycheck-yamllint
  :defer t
  :after flycheck)


(use-package framemove
  :ensure nil
  :config
  (windmove-default-keybindings 'shift)
  ;; Cannot wrap and have framemove do its thing at the same time.
  (setq windmove-wrap-around nil
        framemove-hook-into-windmove t))


(use-package fullframe
  :defer t)


(use-package git-gutter
  :diminish git-gutter-mode
  :hook (prog-mode . git-gutter-mode)
  :bind (("C-c C-n" . git-gutter:next-hunk)
         ("C-c C-p" . git-gutter:previous-hunk))
  :config
  (setq git-gutter:update-interval 0.5))


(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


(use-package gitconfig-mode
  :ensure nil
  :mode ("/gitconfig\\'" . gitconfig-mode))


(use-package gitignore-mode
  :ensure nil
  :mode ("/gitignore\\'" . gitignore-mode))


(use-package company-go
  :defer t)


(use-package go-mode
  :defer t
  :commands (go-mode setup--go-mode setup--go-save-hook)
  :hook ((go-mode . setup--go-mode)
         (go-mode . setup--go-save-hook)
         (go-mode . (lambda()
                      (flycheck-golangci-lint-setup)
                      (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))
  :bind (:map go-mode-map
              ("C-." . company-complete))
  :config
  (defun setup--go-save-hook ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun setup--go-mode ()
    (setq indent-tabs-mode t
          tab-width 4)
    (set (make-local-variable 'company-backends) '(company-go))
    (eldoc-mode 1)
    (company-mode 1)
    (flycheck-mode 1)
    (subword-mode 1)
    (which-function-mode 1)))


(use-package gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))


(use-package grep
  :defer t
  :bind (("M-s g f" . find-grep)
         ("M-s g g" . grep)
         ("M-s g r" . rgrep))
  :hook (grep-mode . turn-on-truncate-lines)
  :config
  (setq grep-files-aliases
        '(("el" . "*.el")
          ("c"  . "*.c")
          ("h"  . "*.h")
          ("cc" . "*.hh *.hpp *.cc *.cpp")
          ("hh" . "*.hh *.hpp *.cc *.cpp")))
  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH -e " . 52))

  (add-to-list 'grep-find-ignored-directories ".direnv")
  (add-to-list 'grep-find-ignored-directories ".git")
  (add-to-list 'grep-find-ignored-directories ".hg")
  (add-to-list 'grep-find-ignored-directories ".idea")
  (add-to-list 'grep-find-ignored-directories "elpa")
  (add-to-list 'grep-find-ignored-directories "target")
  (add-to-list 'grep-find-ignored-directories "vendor")

  (let ((find-command "find . \\( -path '*/CVS' -o -path '*/.hg' -o -path '*/.git' \\) -prune -o -type f -print0"))
    (if macosp
        (setq grep-find-command
              (concat find-command " | xargs -0 " grep-command))
      (setq grep-find-command
            (concat find-command " | xargs -0 -e " grep-command)))))


(use-package grep-a-lot
  :defer t
  :after grep
  :config
  (grep-a-lot-setup-keys))


(use-package groovy-mode
  :mode (("\\.grovvy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))


;; (use-package gud
;;   :commands gud-gdb
;;   ;; Continue / Pause F5.
;;   ;; Step Over F10.
;;   ;; Step Into F11.
;;   ;; Step Out Shift+F11.
;;   ;; Restart Ctrl+Shift+F5.
;;   ;; Stop Shift+F5.
;;   :bind (("<f5>"    . gud-cont)
;;          ("<f10>"   . gud-next)
;;          ("<f11>"   . gud-step)
;;          ("S-<f11>" . gud-finish))
;;   :config
;;   (setq-default gdb-many-windows t
;;                 gdb-use-separate-io-buffer t
;;                 gud-tooltip-mode t))


(use-package helm
  :defer t
  :config
  (helm-autoresize-mode 1))


(use-package helm-projectile
  :bind (("C-x f" . helm-projectile-find-file)
         ("C-c o p" . helm-projectile-find-file))
  :config
  (helm-projectile-on))


(use-package highlight-symbol
  :commands highlight-symbol-nav-mode
  :hook (prog-mode . highlight-symbol-nav-mode))


(use-package hl-line
  :commands hl-line-mode
  :hook (((prog-mode text-mode) . hl-line-mode))
  :bind ("C-c t l" . hl-line-mode))


(use-package ibuffer
  :defer t
  :commands ibuffer
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
  :defer t
  :after ibuffer
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))


(use-package idle-highlight-mode
  :bind ("C-c t i" . idle-highlight-mode))


(use-package iedit
  :bind ("M-RET" . iedit-mode))


(use-package ielm
  :defer t)


(use-package ispell
  :defer t
  :bind (("C-c s b" . ispell-buffer)
         ("C-c s c" . ispell-comments-and-strings)
         ("C-c s d" . ispell-change-dictionary)
         ("C-c s r" . ispell-region)
         ("C-c s w" . ispell-word)
         ("C-c <f11>" . cycle-ispell-languages))
  :config
  (setq ispell-dictionary "english"
        ispell-help-in-bufferp 'electric
        ispell-program-name (if (executable-find "aspell") "aspell" "hunspell")
        ispell-silently-savep t)
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
  :config
  (setf js2-skip-preprocessor-directives t)
  (setq-default js2-additional-externs
                '("$" "unsafeWindow" "localStorage" "jQuery"
                  "setTimeout" "setInterval" "location" "skewer"
                  "console" "phantom"))
  (flycheck-mode 1))


(use-package json-mode
  :preface
  :bind (:map json-mode-map
              ("M-q" . json-mode--reformat-region))
  :config
  (setf json-reformat:pretty-string t
        json-reformat:indent-width 2)
  (defun json-mode--reformat-region ()
    (interactive)
    (if (region-active-p)
        (call-interactively #'json-reformat-region)
      (json-reformat-region (point-min) (point-max)))))


(use-package keyfreq
  :defer t
  :commands keyfreq-mode
  :config
  (setq keyfreq-excluded-commands
        '(backward-char beginning-of-line end-of-line forward-char newline next-line previous-line self-insert-command left-char right-char))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))


(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))


(use-package lisp-mode
  :ensure nil
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c e E" . toggle-debug-on-error)
              ("C-c e e" . eval-last-sexp)
              ("C-c e m" . macrostep-expand)
              ("C-c e r" . eval-region))
  :mode ("Cask" . emacs-lisp-mode)
  :hook (emacs-lisp-mode . setup--emacs-lisp-mode)
  :preface
  (defun setup--emacs-lisp-mode ()
    (add-hook 'after-save-hook 'check-parens nil t)
    (company-mode 1)
    (local-set-key (kbd "C-.") 'company-complete)))


(use-package lsp-headerline
  :ensure nil
  :defer t
  :after lsp-mode)


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp-deferred))
  :init
  (setq read-process-output-max (* 1024 1024))
  :custom
  (lsp-completion-enable t)
  (lsp-completion-provider :capf)
  (lsp-eldoc-enable-hover t)
  (lsp-eldoc-render-all nil)
  (lsp-enable-snippet t)
  (lsp-gopls-complete-unimported t)
  (lsp-gopls-staticcheck t)
  (lsp-headerline-breadcrumb-enable t)
  (lsp-highlight-symbol-at-point t)
  (lsp-idle-delay 0.6)
  (lsp-keymap-prefix "C-c l")
  (lsp-prefer-capf t)
  (lsp-pyls-plugins-flake8-enabled t)
  :config
  (setq lsp-go-use-gofumpt t
        lsp-prefer-flymake nil)
  (lsp-enable-which-key-integration t)
  (lsp-register-custom-settings
   '(
     ("gopls.gofumpt" t)
     ("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)

     ;; https://github.com/palantir/python-language-server/blob/develop/vscode-client/package.json
     ("pyls.plugins.pydocstyle.enabled" t t)
     ("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)
     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  )


(use-package lsp-lens
  :ensure nil
  :defer t
  :after lsp-mode)


(use-package lsp-java
  :defer t
  :init
  ;; workaround https://github.com/Alexander-Miller/treemacs/issues/1017#issuecomment-1515602288
  (add-to-list 'image-types 'svg)
  :config
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-Djava.awt.headless=true")))


(use-package lsp-treemacs
  :after lsp-mode
  :commands lsp-treemacs-errors-list)


(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))


(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))


(use-package magit
  :defer t
  :commands magit-status
  :hook (magit-mode . hl-line-mode)
  :config
  (setq ;; magit-completing-read-function 'ivy-completing-read
   magit-diff-refine-hunk t
   magit-push-always-verify nil
   magit-repository-directories '("~/.emacs.d")
   magit-restore-window-configuration t
   magit-section-initial-visibility-alist '((stashes . show) (unpushed . show) (upstream . show) (untracked . show))
   magit-status-buffer-switch-function 'switch-to-buffer)
  (defadvice magit-diff-working-tree (after magit-diff-focus activate)
    "After execution, select the magit-diff buffer in the current window."
    (other-window 1))
  (fullframe magit-status-setup-buffer magit-mode-quite-window))


(use-package magit-blame
  :ensure nil
  :after magit)


(use-package man
  :commands man
  :config
  (setq Man-notify-method 'pushy))


(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"))


(use-package misc
  :ensure nil
  :bind (("M-z" . zap-up-to-char)
         ("M-Z" . zap-to-char))
  :commands zap-up-to-char)


(use-package move-text
  :bind (("<M-up>" . move-text-up)
         ("<M-down>" . move-text-down)))


(use-package multiple-cursors
  :defer t
  :commands (mc/mark-next-like-this)
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-c C->"     . mc/unmark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/unmark-previous-like-this)
         ("C-c <"       . mc/mark-all-like-this)
         ("C-|"         . mc/edit-lines)
         ("<M-S-up>"    . mc/mark-previous-like-this)
         ("<M-S-down>"  . mc/mark-next-like-this)
         ("s-<mouse-1>" . mc/add-cursor-on-click)))


(use-package newcomment
  :ensure nil
  :defer t
  :commands comment-line
  :bind ("C-;" . comment-line))


(use-package nlinum
  :defer t
  :bind ("C-c t l" . nlinum-mode))


(use-package nuke-whitespace
  :ensure nil
  :bind ("C-c t n" . nuke-trailing-whitespace)
  :config
  (progn
    (remove-hook 'write-file-hooks 'nuke-trailing-whitespace)
    (setq nuke-trailing-whitespace-always-major-modes
          (remove 'python-mode nuke-trailing-whitespace-always-major-modes))
    ))


(use-package nxml-mode
  :ensure nil
  :defer t
  :commands nxml-mode
  :init
  (defalias 'xml-mode 'nxml-mode)
  :config
  (defun nxml-set-indentation (level)
    "Set indentation LEVEL in nxml-mode. Default LEVEL is 2."
    (interactive "p")
    (if (derived-mode-p 'nxml-mode)
        (let ((default-level 2))
          (progn
            (setq level (if (= level 1) default-level level))
            (message "NXML indentation is %s." level)
            (setq nxml-child-indent level
                  nxml-attribute-indent level)))))

  (nxml-set-indentation 2)

  (setq nxml-auto-insert-xml-declaration-flag nil
        nxml-bind-meta-tab-to-complete-flag t
        nxml-slash-auto-complete-flag t
        nxml-syntax-highlight-flag t
        rng-nxml-auto-validate-flag nil)
  (push '("<\\?xml" . nxml-mode) magic-mode-alist))


(use-package openwith
  :defer t
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
  :defer t
  :commands org-mode
  :bind (("M-m" . org-capture)
         ("C-c a" . org-agenda))
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-replace-disputed-keys t
        org-export-backends '(ascii html md reveal))
  :hook (org-mode . setup--org-mode)
  :config
  (setq
   org-agenda-span 14
   org-agenda-start-on-weekday nil
   org-clock-history-length 20
   org-clock-in-resume t
   org-habit-show-all-today nil
   org-habit-show-habits-only-for-today t
   org-hide-leading-stars nil
   org-level-color-stars-only t
   org-log-done 'time
   org-log-into-drawer t
   org-odd-levels-only nil
   org-reverse-note-order t
   org-src-fontify-natively t
   org-src-window-setup 'current-window
   org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
   org-time-stamp-rounding-minutes '(0 5)
   org-todo-keywords '((sequence "TODO(t!)" "VERIFY(v!)" "|" "DONE(d!)" "CANCELED(c@)"))
   org-treat-insert-todo-heading-as-state-change t
   org-use-speed-commands nil)

  (setq org-agenda-prefix-format
        '((agenda  . " %i %-12:c%?-12t% s")
          (todo  . " %i %-12:c")
          (tags  . " %i %-12:c")
          (search . " %i %-12:c")))

  (add-to-list 'org-modules 'org-habit t)

  (setq org-agenda-custom-commands
        '(("h" "Daily habits"
           ((agenda ""))
           ((org-agenda-show-log t)
            (org-agenda-ndays 7)
            (org-agenda-log-mode-items '(state))
            (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
          ))

  (fullframe org-agenda quit-window)

  ;; (setq org-clock-persist t
  ;;       org-clock-persist-query-resume nil)
  ;; (org-clock-persistence-insinuate)

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-tag ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit fixed-pitch)))))

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

    (define-key org-read-date-minibuffer-local-map (kbd "S-<left>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "S-<right>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "S-<up>") (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
    (define-key org-read-date-minibuffer-local-map (kbd "S-<down>") (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))

    (local-set-key [M-up]   'outline-previous-visible-heading)
    (local-set-key [M-down] 'outline-next-visible-heading)))


(use-package ox-reveal
  :defer t)


(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))


(use-package pdf-tools
  :defer t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :config
  (progn
    (setq-default pdf-view-display-size 'fit-width)
    (setq pdf-annot-activate-created-annotations t
          pdf-view-resize-factor 1.10)
    (pdf-tools-install)))


(use-package php-mode
  :disabled t
  :init
  (setq php-extra-constants '())
  :hook (php-mode . setup--php-mode)
  :config
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


(use-package plantuml-mode
  :mode "\\.plantuml\\'")
;; (catch 'loop
;;   (dolist (jar-file (append (file-expand-wildcards "/usr/share/java/plantuml*.jar")
;;                             (file-expand-wildcards "/usr/local/Cellar/plantuml/*/libexec/plantuml.jar")))
;;     (message "Found plantuml: %s" jar-file)
;;     (use-package plantuml-mode
;;       :config
;;       (setq plantuml-jar-path jar-file))
;;     (throw 'loop nil)))


(use-package printing
  :defer t
  :config
  (pr-update-menus t)
  (setq lpr-command          "lpr"
        lpr-headers-switches "-h"
        ps-paper-type         'a4
        ps-print-color-p      nil
        ps-number-of-columns  2
        ps-landscape-mode     t))


(use-package projectile
  :defer t
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
  :hook (protobuf-mode . (lambda () (c-add-style "my-style" my-protobuf-style t))))


(use-package pyvenv
  :defer t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))


(use-package python
  :commands setup--python-mode
  :bind (:map python-mode-map
              ("C-c C-z" . python-shell-switch-to-shell)
              ("C-c z" . run-python)
              ("C-." . company-complete)
              ("C-c B" . blacken-buffer)
              ("C-c I" . python-isort-buffer)
              ("C-c 2 3" . python-2to3-current-buffer)
              ("<f9>" . py-run)
              ("<S-f9>" . pdb)
              ("<C-f9>" . compile)
              ("<M-f9>" . recompile))
  :hook ((python-mode . setup--python-mode))
  :config
  (defadvice pdb (before gud-query-cmdline activate)
    "Provide a better default command line when called interactively."
    (interactive
     (list (gud-query-cmdline 'pdb
                              (file-name-nondirectory buffer-file-name)))))

  (defun py-run ()
    "Run python on the file in the current buffer."
    (interactive)
    (compile (format "python \"%s\"" (buffer-file-name))))

  (defun flycheck-use-python-version (version)
    (interactive
     (list
      (completing-read
       "Which Python version should flycheck use: "
       '("python" "python2" "python3") nil t)))
    (setq
     flycheck-python-flake8-executable version
     flycheck-python-pycompile-executable version
     flycheck-python-pylint-executable version))

  (defun setup--python-mode ()
    ;;(setq py-python-command-args '( "-colors" "Linux"))

    (modify-syntax-entry ?\_ "_" python-mode-syntax-table)

    (subword-mode 1)
    (which-function-mode 1)

    (let ((activate (if (file-remote-p default-directory) -1 1)))
      (flycheck-mode activate)
      (company-mode activate))

    (idle-highlight-mode 1)

    (require 'sphinx-doc)
    (sphinx-doc-mode 1)

    (set (make-variable-buffer-local 'outline-regexp) "def\\|class ")
    (set (make-variable-buffer-local 'indent-tabs-mode) nil)))


(use-package python-isort
  :after python)


(use-package rainbow-mode
  :defer t)


(use-package re-builder
  :defer t
  :config
  (setq reb-re-syntax 'string))


(use-package rg
  :defer t
  :if (executable-find "rg")
  :commands (rg
             rg-project
             rg-literal
             rg-dwim
             rg-dwim-project-dir
             rg-dwim-current-dir
             rg-dwim-current-file)
  :bind (("M-s r r" . rg)
         ("M-s r p" . rg-project))
  :bind (:map rg-mode-map
              ("C-c '" . wgrep-change-to-wgrep-mode)
              ("q" . kill-buffer-and-window)))

(use-package rg-menu
  :ensure nil
  :after rg
  :commands (rg-menu rg-enable-menu))

(use-package wgrep-rg
  :ensure nil
  :after rg
  :commands (wgrep-rg-setup)
  :hook (rg-mode . wgrep-rg-setup))


(use-package rst
  :defer t
  :hook (rst-mode . setup--rst-mode)
  :config
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
  :hook (ruby-mode . setup--ruby-mode)
  :config
  (use-package ruby-end
    :diminish ruby-end-mode)
  (use-package robe
    :diminish robe-mode))


(use-package savehist
  :unless noninteractive
  :init
  (setq savehist-ignored-variables '(file-name-history))
  (savehist-mode))


(use-package saveplace
  :unless noninteractive
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  :config
  (save-place-mode 1))


(use-package scss-mode
  :defer t
  :commands scss-mode
  :mode ("\\.scss\\'" . scss-mode)
  :hook (scss-mode . turn-on-rainbow-mode))


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


(use-package simple
  :ensure nil
  :bind (("M-SPC" . cycle-spacing)))


(use-package smartparens
  :defer t)


(use-package smerge-mode
  :defer t
  :init
  (setq smerge-command-prefix "\C-cv"))


(use-package solarized-theme
  :ensure t
  :if (display-graphic-p)
  :config
  (load-theme 'solarized-dark :no-confirm)
  :custom-face
  (variable-pitch ((t (:height 170))))
  (fixed-pitch ((t (:height 150))))
  (default ((t (:height 150))))

  (cursor ((t (:foreground "white" :background "firebrick" :inverse-video t))))
  ;; (region ((t (:foreground "black" :background "#b5d5ff"))))
  (match ((t (:background "#FFFF66" :foreground "black" :weight bold))))
  (highlight ((t :background "black" :underline nil)))

  ;; diff
  (diff-added ((t (:foreground "green4" :underline nil))))
  (diff-changed ((t (:foreground "blue3"))))
  (diff-removed ((t (:foreground "red4" :underline nil))))
  (diff-refine-added ((t (:background "#446644" :foreground "green"))))
  (diff-refine-changed ((t (:background "#ddddff" :foreground "blue"))))
  (diff-refine-removed ((t (:background "#664444" :foreground "red"))))

  ;; magit
  (magit-item-highlight ((t nil)))

  ;; hl-line-mode
  (hl-line-face ((t (:background "gray10"))))

  ;; idle-highlight
  (idle-highlight ((t (:foreground "yellow" :background "black"))))

  ;; isearch
  (isearch
   ((t (:foreground "black" :background "yellow" :bold t))))
  (isearch-lazy-highlight-face
   ((t (:foreground "black" :background "yellow"))))
  (isearch-fail
   ((t (:foreground "red" :background "yellow" :bold t))))

  ;; show-paren
  (show-paren-match
   ((t (:foreground "black" :background "dark green" :bold t))))
  (show-paren-mismatch
   ((t (:foreground "yellow" :background "indian red" :bold t)))))


(use-package speedbar
  :disabled t
  :config
  (setq speedbar-default-position 'left
        speedbar-show-unknown-files t
        speedbar-update-flag t))


(use-package sphinx-doc
  :defer t
  :diminish sphinx-doc-mode)


(use-package sql
  :defer t
  :hook (sql-interactive-mode . setup--sql-interactive-mode)
  :config
  (setq plsql-indent 2)

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


(use-package subword
  :bind (:map subword-mode-map
              ("<M-left>"      . subword-backward)
              ("<M-right>"     . subword-forward)
              ("<C-left>"      . subword-backward)
              ("<C-right>"     . subword-forward)
              ("<C-backspace>" . subword-backward-kill))
  :diminish subword-mode)


(use-package term
  :defer t
  :hook (term-mode . setup--term)
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
    (goto-address-mode)
    ;; (define-key term-raw-map (kbd "M-o") 'other-window)
    ;; (define-key term-raw-map (kbd "M-p") 'term-send-up)
    ;; (define-key term-raw-map (kbd "M-n") 'term-send-down)
    ;; (define-key term-raw-map (kbd "C-y") 'setup--term-paste))
    ))


(use-package terraform-mode
  :defer t
  :mode "\\.tf\\'"
  :hook (terraform-mode . terraform-format-on-save-mode)
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.tfstate\\'" . json-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.json.tftemplate\\'" . json-mode))
  )


(use-package time
  :defer t
  :config
  (setq display-time-world-time-format "%Y-%m-%d %H:%M %Z"
        display-time-world-list '(("America/Los_Angeles" "San Fransisco")
                                  ("America/New_York" "New York")
                                  ("Europe/London" "London")
                                  ("Europe/Stockholm" "Stockholm")
                                  ("Asia/Tokyo" "Tokyo"))))


(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode))
  :commands (toml-mode))


(use-package tramp
  :defer t
  :config
  (put 'temporary-file-directory 'standard-value '("/tmp"))
  (setq tramp-auto-save-directory "~/.cache/emacs/backups"
        tramp-persistency-file-name "~/.emacs.d/data/tramp"))

(use-package uniquify
  :ensure nil
  :defer t
  :config
  (setq uniquify-buffer-name-style 'forward))


(use-package vc
  :defer t
  :config
  (setq vc-command-messages    t
        vc-follow-symlinks     t
        vc-consult-headers     t
        vc-keep-workfiles      t
        vc-make-backup-files   nil
        vc-dired-terse-display nil
        vc-dired-recurse       nil)
  (define-key vc-prefix-map "e" 'ediff-revision-current-buffer)
  (define-key vc-prefix-map "R" 'vc-resolve-conflicts)

  (fullframe vc-dir quit-window))


;; (use-package vc-hg
;;   :after vc
;;   :config
;;   (setq hg-commit-allow-empty-message t
;;         vc-hg-diff-switches "--text"))


(use-package vimrc-mode
  :mode ("\\.vim\\(rc\\)?\\'" . vimrc-mode))


(use-package vlf-setup
  :ensure nil
  :defer t)


(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode)
         ("\\.rhtml\\'" . web-mode)
         ("\\.\\(php\\|inc\\)\\'" . web-mode))
  :config
  (setq web-mode-enable-block-face t
        web-mode-enable-part-face t
        web-mode-disable-css-colorization nil
        web-mode-disable-auto-pairing nil))


(use-package webjump
  :defer t
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
          ("Python 2" .
           [simple-query "http://docs.python.org/2.7"
                         "http://docs.python.org/release/2.7/search.html?q=" ""])
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
                     "&words=")
            ""]))))


(use-package wgrep
  :defer t)


(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))


(use-package ws-butler
  :defer t
  :diminish ws-butler-mode
  :hook ((org-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)
         (text-mode . ws-butler-mode))
  :init
  (setq ws-butler-keep-whitespace-before-point nil))


(use-package xclip
  :defer t
  :config
  (progn
    (xclip-mode 1)))


(use-package yaml-mode
  :defer t
  :commands (yaml-mode setup--yaml-mode)
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :hook (yaml-mode . flycheck-mode))


(use-package yasnippet
  :defer t
  :bind (("C-c t y" . company-yasnippet))
  :commands (snippet-mode yas-expand yas-minor-mode)
  :diminish yas-minor-mode
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode)
  :init
  (setq yas-verbosity 0)
  :config
  (yas-global-mode 1)
  (setq-default yas-prompt-functions
                '(yas/ido-prompt yas/completing-prompt))
  (load "snippet-helpers"))


(use-package yasnippet-snippets
  :after yasnippet
  :config (yasnippet-snippets-initialize))


(use-package zeal-at-point
  :defer t
  :bind ("C-c t z" . zeal-at-point)
  :config
  (setq zeal-at-point-mode-alist
        (delete
         (assoc 'python-mode zeal-at-point-mode-alist)
         zeal-at-point-mode-alist))
  (add-to-list 'zeal-at-point-mode-alist
               '(python-mode . "python")))


(provide 'setup)
