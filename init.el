;; init.el -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)

(setq byte-compile-warnings '(cl-functions)
      warning-suppress-log-types '((package reinitialization)))

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)

(menu-bar-mode 0)
(set-cursor-color "red")
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1))


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

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      package-archive-priorities
      '(("melpa" . 10)
        ("elpa" . 5)
        ("nongnu" . 0)
        ("gnu" . 0)))

;; (package-initialize)

;; Bootstrap use-package
(setq use-package-always-ensure t
      use-package-compute-statistics t
      use-package-enable-imenu-support t
      use-package-expand-minimally t
      use-package-verbose t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package bind-key)
(use-package diminish)
(use-package s)
(use-package f)
(use-package dash)

;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(dolist (fn '("defuns" "defaults" "key-bindings" "setup"))
  (load (expand-file-name fn user-emacs-directory)))
(when (eq system-type 'darwin)
  (load (expand-file-name "macos" user-emacs-directory)))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(custom-set-faces
 '(variable-pitch ((t (:height 170))))
 '(fixed-pitch ((t (:height 150))))
 '(default ((t (:height 150)))))

(load (expand-file-name "user" user-emacs-directory) 'noerror)

;;; init.el ends here
