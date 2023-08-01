;; quick.el -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)

(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      package-enable-at-startup t
      package-native-compile t)

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

;;(package-initialize)

;; Bootstrap use-package
(setq package-native-compile t
      use-package-always-ensure nil
      use-package-compute-statistics t
      use-package-enable-imenu-support t
      use-package-verbose t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(dolist (fn '("defuns" "defaults" "key-bindings"))
  (load (expand-file-name fn user-emacs-directory)))
(when (eq system-type 'darwin)
  (load (expand-file-name "macos" user-emacs-directory)))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(load (expand-file-name "user" user-emacs-directory) 'noerror)

(setq-default ediff-ignore-similar-regions t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))


(use-package dumb-jump
  :init
  (setq dumb-jump-force-searcher 'rg
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  :hook (xref-backend-functions . dumb-jump-xref-activate))


(use-package magit
  :defer t)

(provide 'quick)
;;; quick.el ends here
