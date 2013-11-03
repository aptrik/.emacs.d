(let ((min-version "24.3"))
  (when (version< emacs-version min-version)
    (error "Emacs %s is required!" min-version)))

(dolist (mode '(tool-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'right))

(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;; Prepare load-path.
(add-to-list 'load-path user-emacs-directory)
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'defuns)
(require 'autoloads)

(when (display-graphic-p)
  (exec-path-from-shell-initialize))

(add-to-list 'custom-theme-load-path user-emacs-directory)
(load-theme 'aptrik t)

;; Set up modes.
(let ((s-dir (file-name-as-directory "setup")))
  (dolist (x '("default"
               "c" ;; also for C++ and IDL
               "calendar"
               "dired"
               "ediff"
               "elisp"
               ;; "hippie"
               ;; "ido"
               "javascript"
               ;; "mail"
               "moz"
               "nxml"
               "org"
               ;; "perl"
               "python"
               "ruby"
               "shell"
               "web"
               ;; "yasnippet"
               ))
    (load (concat s-dir x))))

(require 'mode-mappings)

;; Re-enable commands.
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'key-bindings)

(load "user" 'noerror)
