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
(require 'use-package)

(require 'defuns)
(require 'defaults)
(require 'setup)

(when (display-graphic-p)
  (exec-path-from-shell-initialize))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'key-bindings)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'aptrik t)

(load "user" 'noerror)
