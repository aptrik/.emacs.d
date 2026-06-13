;; init.el -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      initial-scratch-message nil)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-cursor-color "red")
(tool-bar-mode -1)
(tooltip-mode -1)


;;
;; Compare with:
;; emacs -nw -Q --eval='(message "%s" (emacs-init-time))'
;;
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "+++ Emacs ready in %.1f seconds (%d garbage collections)"
;;                      (float-time
;;                       (time-subtract after-init-time before-init-time))
;;                      gcs-done)))

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-vc-git-default-clone-depth 1)
(straight-register-package '(org :type built-in))
(straight-use-package 'use-package)

(setq use-package-compute-statistics t
      use-package-enable-imenu-support t
      use-package-expand-minimally t
      use-package-verbose t)

(require 'use-package)
(use-package bind-key :straight t)
(use-package diminish :straight t)
(use-package s :straight t)
(use-package f :straight t)
(use-package dash :straight t)

;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

;; (require 'emacs-load-time)

(dolist (fn '("defuns" "defaults" "key-bindings" "setup"))
  (load (expand-file-name fn user-emacs-directory) nil 'nomessage))
(when (eq system-type 'darwin)
  (load (expand-file-name "macos" user-emacs-directory) nil 'nomessage))

(custom-set-faces
 '(variable-pitch ((t (:height 170))))
 '(fixed-pitch ((t (:height 150))))
 '(default ((t (:height 150)))))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

(load (expand-file-name "user" user-emacs-directory) 'noerror 'nomessage)

;;; init.el ends here
