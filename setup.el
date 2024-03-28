;; -*- lexical-binding: t -*-

;;-----------------------------------------------------------------------------
;;; General settings

(use-package emacs
  :init
  (setq completion-cycle-threshold 3
        tab-always-indent t))


;;-----------------------------------------------------------------------------
;;; Completion

(use-package consult
  :ensure t
  :after vertico
  :bind (("C-x b"   . consult-buffer)
         ("M-g g"   . consult-goto-line)
         ;; ("M-g f"   . consult-flymake)
         ("M-g i"   . consult-imenu)
         ("M-s l"   . consult-line)
         ("M-s g"   . consult-ripgrep)
         ("M-s M-s" . consult-outline))
  :init
  (defvar consult--source-hotspots
    `(:name "Hotspot directories"
      :narrow ?\h
      :face   consult-file
      :items  ,#'hotspot-generate-directories
      :action ,(lambda (d) (find-file d)))
    "Hotspot candidates source for `consult-buffer'.")
  (setq consult-buffer-sources
  '(consult--source-hidden-buffer
    consult--source-modified-buffer
    consult--source-buffer
    consult--source-recent-file
    consult--source-file-register
    ;; consult--source-bookmark
    consult--source-project-buffer-hidden
    consult--source-project-recent-file-hidden
    consult--source-hotspots)))


(use-package consult-project-extra
  :ensure t
  :bind (("C-c f p" . consult-project-extra-find)))


(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  :bind (("C-." . completion-at-point)
         ("C-c ." . completion-at-point))
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))


(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless flex)))


(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("<tab>" . vertico-insert)
              ("<down>" . vertico-next)
              ("<up>" . vertico-previous)
              ("C-M-<down>" . vertico-next-group)
              ("C-M-<up>" . vertico-previous-group))
  :custom
  (vertico-cycle t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :init
  (vertico-mode))


;;-----------------------------------------------------------------------------
;;; Configuration in alphabetical order

(use-package aggressive-indent
  :disabled
  :diminish
  :hook (emacs-lisp-mode . aggressive-indent-mode))


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


(use-package copy-as-format
  :bind (("C-c t w g" . copy-as-format-github)
         ("C-c t w j" . copy-as-format-jira)
         ("C-c t w m" . copy-as-format-markdown)
         ("C-c t w o" . copy-as-format-org-mode)
         ("C-c t w r" . copy-as-format-rst)
         ("C-c t w s" . copy-as-format-slack)))


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


(use-package direnv
  :defer t
  :hook ((python-base-mode . direnv-mode))
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


(use-package ffap
  :ensure nil
  :bind ("C-c f f" . find-file-at-point))


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


(use-package gitconfig-mode
  :ensure nil
  :mode ("/gitconfig\\'" . gitconfig-mode))


(use-package gitignore-mode
  :ensure nil
  :mode ("/gitignore\\'" . gitignore-mode))


(use-package go-mode
  :defer t
  :commands (go-mode setup--go-mode setup--go-save-hook)
  :hook ((go-mode . setup--go-mode)
         (go-mode . setup--go-save-hook)
         (go-mode . (lambda()
                      (flycheck-golangci-lint-setup)
                      (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint)))))))))
  :config
  (defun setup--go-save-hook ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun setup--go-mode ()
    (setq indent-tabs-mode t
          tab-width 4)
    (eldoc-mode 1)
    (flycheck-mode 1)
    (subword-mode 1)
    (which-function-mode 1)))


(use-package gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))


(use-package groovy-mode
  :mode (("\\.grovvy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))


(use-package highlight-symbol
  :commands highlight-symbol-nav-mode
  :hook (prog-mode . highlight-symbol-nav-mode))


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
    (add-hook 'after-save-hook 'check-parens nil t)))


(use-package lsp-headerline
  :ensure nil
  :defer t
  :after lsp-mode)


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind ((:map lsp-mode-map
               ("M-<return>" . lsp-execute-code-action)))
  :init
  (setq read-process-output-max (* 3 1024 1024))
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
  (lsp-enable-which-key-integration t)
  (lsp-register-custom-settings
   '(
     ("gopls.gofumpt" t)
     ("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("pylsp.plugins.rope_autoimport.enabled" t t)
     ("pylsp.plugins.rope_autoimport.completions.enabled" t t)
   ))
  (setq lsp-prefer-flymake nil
        lsp-go-use-gofumpt t
        lsp-pylsp-plugins-black-enabled t))


(use-package lsp-java
  :disabled
  :ensure t
  :defer t
  :config
  (require 'lsp-java-boot)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  (setq lsp-java-vmargs
        (list
         "-noverify"
         "-Xmx3G"
         "-XX:+UseG1GC"
         "-XX:+UseStringDeduplication"
         "-Djava.awt.headless=true")))


(use-package lsp-languages
  :ensure nil
  :hook ((go-mode . lsp-deferred)
         ;;(java-mode . lsp-deferred)
         (python-base-mode . lsp-deferred)
         ;;(xml-mode . lsp-deferred)
         ;;(web-mode . lsp-deferred)
         )
  ;; Workaround for "Invalid Image type: 'gif'"
  ;; - https://github.com/emacs-lsp/lsp-mode/issues/4054
  ;; - https://github.com/Alexander-Miller/treemacs/issues/1017#issuecomment-1515602288
  :init
  (add-to-list 'image-types 'gif)
  (add-to-list 'image-types 'svg))


(use-package lsp-lens
  :ensure nil
  :defer t
  :after lsp-mode)


(use-package lsp-treemacs
  :ensure t
  :after (lsp-mode treemacs)
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
         ("M-9" . lsp-treemacs-errors-list)))


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after (lsp-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-doc-delay 1.5
        lsp-ui-doc-position 'bottom
	lsp-ui-doc-max-width 100))


(use-package magit
  :defer t
  :bind (("C-x v SPC" . magit-status))
  :commands magit-status
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
  :bind (("C-c <" . mc/mark-all-like-this)
         ("C-|" . mc/edit-lines)
         ("<M-S-up>" . mc/mark-previous-like-this)
         ("<M-S-down>" . mc/mark-next-like-this)
         ("<M-S-C-up>" . mc/unmark-next-like-this)
         ("<M-S-C-down>" . mc/unmark-previous-like-this)
         ("S-<mouse-1>" . mc/add-cursor-on-click)))


(use-package newcomment
  :ensure nil
  :defer t
  :commands comment-line
  :bind ("C-;" . comment-line))


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


(use-package org
  :defer t
  :commands org-mode
  :bind (("M-m" . org-capture)
         ("C-c a" . org-agenda))
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-replace-disputed-keys t
        org-export-backends '(ascii html md))
  :hook (org-mode . setup--org-mode)
  :config
  (setq
   org-agenda-span 14
   org-agenda-start-on-weekday nil
   org-ascii-indented-line-width nil
   org-clock-history-length 20
   org-clock-idle-time nil
   org-clock-in-resume t
   org-clock-persist 'history
   org-clock-persist-query-resume nil
   org-clock-rounding-minutes 5
   org-duration-format 'h:mm
   org-habit-show-all-today nil
   org-habit-show-habits-only-for-today t
   org-hide-leading-stars nil
   org-level-color-stars-only t
   org-log-done 'time
   org-log-into-drawer t
   org-odd-levels-only nil
   org-reverse-note-order t
   org-src-fontify-natively t
   org-src-preserve-indentation t
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

  (org-clock-persistence-insinuate)
  (fullframe org-agenda quit-window)

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


(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  (show-paren-mode 1))


(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . (lambda () (setq-local show-trailing-whitespace t)))
         (prog-mode . (lambda () (electric-indent-local-mode -1)))
         (prog-mode . (lambda () (electric-pair-local-mode -1)))))


(use-package protobuf-mode
  :mode ("\\.proto\\'" . protobuf-mode)
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  :hook (protobuf-mode . (lambda () (c-add-style "my-style" my-protobuf-style t))))


(use-package pulse
  :defer t
  :init (defun pulse-line (&rest _)
          (pulse-momentary-highlight-one-line (point)))
  (dolist (command '(other-window
                     windmove-do-window-select
                     mouse-set-point
                     mouse-select-window))
    (advice-add command :after #'pulse-line)))


(use-package pyvenv
  :defer t
  :init
  (setenv "WORKON_HOME" "~/.pyenv/versions"))


(use-package python
  :commands setup--python-mode
  :bind (:map python-mode-map
              ("C-c C-z" . python-shell-switch-to-shell)
              ("C-c z" . run-python)
              ("C-c B" . blacken-buffer)
              ("C-c I" . python-isort-buffer)
              ("C-c 2 3" . python-2to3-current-buffer)
              ("<f9>" . py-run)
              ("<S-f9>" . pdb)
              ("<C-f9>" . compile)
              ("<M-f9>" . recompile))
  :hook ((python-base-mode . setup--python-mode)
         ;; (python-base-mode . setup--python-save-hook)
         )
  :config
  (defun setup--python-save-hook ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

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
    (idle-highlight-mode 1)

    (require 'sphinx-doc)
    (sphinx-doc-mode 1)

    (set (make-variable-buffer-local 'outline-regexp) "def\\|class ")
    (set (make-variable-buffer-local 'indent-tabs-mode) nil)))


(use-package python-isort
  :after python)


(use-package rst
  :defer t
  :hook (rst-mode . setup--rst-mode)
  :config
  (defun setup--rst-mode ()
    (sphinx-mode 1)))


(use-package savehist
  :unless noninteractive
  :init
  (savehist-mode))


(use-package saveplace
  :unless noninteractive
  :init
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  :config
  (save-place-mode 1))


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


(use-package sphinx-doc
  :defer t
  :diminish sphinx-doc-mode)


(use-package subword
  :bind (:map subword-mode-map
              ("<M-left>"      . subword-backward)
              ("<M-right>"     . subword-forward)
              ("<C-left>"      . subword-backward)
              ("<C-right>"     . subword-forward)
              ("<C-backspace>" . subword-backward-kill))
  :diminish subword-mode)


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
  (setq world-clock-time-format "%Y-%m-%d %H:%M %Z"
        world-clock-list
        '(("America/Seattle" "Seattle")
          ("America/New_York" "New York")
          ("Europe/Stockholm" "Stockholm")
          ("Asia/Singapore" "Singapore")
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


(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))


(use-package treesit-auto
  :disabled
  ;; Execute once
  ;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
  :ensure t
  :config
  (setq treesit-auto-install 'prompt
        treesit-language-source-alist
        '(
          (bash "https://github.com/tree-sitter/tree-sitter-bash")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (go-mode . go-ts-mode)
          (java-mode . java-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (toml-mode . toml-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


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
          ("Python" .
           [simple-query "http://docs.python.org/3.12.0"
                         "http://docs.python.org/release/3.12.0/search.html?q=" ""])
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


;;-----------------------------------------------------------------------------
;;; Themes

(use-package solarized-theme
  :ensure t
  ;; :if (display-graphic-p)
  :config
  (load-theme 'solarized-selenized-dark :no-confirm)
  :custom-face
  (default ((t (:background "#002b36")))) ;; same as iTerm/Profiles/Color/Background

  ;; (variable-pitch ((t (:height 170))))
  ;; (fixed-pitch ((t (:height 150))))
  ;; (default ((t (:height 150))))

  (cursor ((t (:foreground "white" :background "firebrick" :inverse-video t))))
  ;; (region ((t (:foreground "black" :background "#b5d5ff"))))
  (match ((t (:background "#FFFF66" :foreground "black" :weight bold))))
  ;; (highlight ((t :background "black" :underline nil)))

  ;; diff
  ;; (diff-added ((t (:foreground "green4" :underline nil))))
  ;; (diff-changed ((t (:foreground "blue3"))))
  ;; (diff-removed ((t (:foreground "red4" :underline nil))))
  ;; (diff-refine-added ((t (:background "#446644" :foreground "green"))))
  ;; (diff-refine-changed ((t (:background "#ddddff" :foreground "blue"))))
  ;; (diff-refine-removed ((t (:background "#664444" :foreground "red"))))

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
   ((t (:foreground "black" :background "green" :bold t))))
  (show-paren-mismatch
   ((t (:foreground "yellow" :background "indian red" :bold t)))))


(provide 'setup)
