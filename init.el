;; -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)


(setq byte-compile-warnings '(cl-functions)
      warning-suppress-log-types '((package reinitialization)))

(dolist (mode '(tool-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)
(menu-bar-mode -1)


;;
;; Compare with:
;; emacs -nw -Q --eval='(message "%s" (emacs-init-time))'
;;
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "+++ Emacs ready in %.1f seconds (%d garbage collections)"
                     (float-time
                      (time-subtract after-init-time before-init-time))
                     gcs-done)))

(when (eval-when-compile (version< emacs-version "27"))
  (package-initialize))

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; Setup up use-package.
(setq use-package-enable-imenu-support t
      use-package-verbose nil)
(eval-when-compile
  (require 'use-package))
(setq use-package-compute-statistics t)
(require 'bind-key)
(require 'diminish)

;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(defun load-local (file)
  (load (expand-file-name file user-emacs-directory)))

(dolist (fn '("defuns" "defaults" "key-bindings" "setup"))
  (load-local fn))
(when (eq system-type 'darwin)
  (load-local "macos"))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(custom-set-faces
 '(variable-pitch ((t (:height 180))))
 '(fixed-pitch ((t (:height 170))))
 '(default ((t (:height 170)))))

(load (expand-file-name "user" user-emacs-directory) 'noerror)
