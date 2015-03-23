;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)

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

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

(when (display-graphic-p)
  (exec-path-from-shell-initialize))

;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(dolist (fn '("defuns" "defaults" "setup" "key-bindings"))
  (load (expand-file-name fn user-emacs-directory)))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'aptrik t)

(load (expand-file-name "user" user-emacs-directory) 'noerror)
