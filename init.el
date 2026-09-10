;; init.el -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)

(setq inhibit-default-init t
      inhibit-splash-screen t
      inhibit-startup-buffer-menu t
      inhibit-startup-message t
      initial-scratch-message nil)

(defvar my--init-start (current-time))
(add-hook 'window-setup-hook
          (lambda ()
            (message "*** Total startup: %.2fs (init: %s)"
                     (float-time (time-subtract (current-time) my--init-start))
                     (emacs-init-time))))

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

;; Bootstrap elpaca
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq elpaca-lock-file (expand-file-name "elpaca-lock.el" user-emacs-directory))

;; use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

(setq use-package-compute-statistics t
      use-package-enable-imenu-support t
      use-package-expand-minimally t
      use-package-verbose t)

(use-package bind-key)
(use-package diminish :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)
(use-package dash :ensure t)
(elpaca-wait)

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
