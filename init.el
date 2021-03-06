;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)

(setq warning-suppress-log-types '((package reinitialization)))

(let ((min-version "25.1"))
  (when (version< emacs-version min-version)
    (error "Emacs %s is required!" min-version)))

(dolist (mode '(tool-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

;;
;; Compare with:
;; emacs -q --eval='(message "%s" (emacs-init-time))'
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

(setq use-package-enable-imenu-support t)
(eval-when-compile
  (require 'use-package))

(require 'bind-key)
(require 'diminish)

;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(dolist (fn '("defuns" "defaults" "key-bindings" "setup"))
  (load (expand-file-name fn user-emacs-directory)))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(load (expand-file-name "user" user-emacs-directory) 'noerror)
