;; -*- lexical-binding: t -*-

;; (setq debug-on-error t)
;; (setq debug-on-signal t)
;; (setq debug-on-quit t)


;; Prepare load-path.
(let ((dir (expand-file-name "lisp" user-emacs-directory)))
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir t "\\w+"))
    (when (file-directory-p f)
      (add-to-list 'load-path f))))

(dolist (fn '("defuns" "defaults" "key-bindings"))
  (load (expand-file-name fn user-emacs-directory)))

;; Set customization file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; (load (expand-file-name "user" user-emacs-directory) 'noerror)

;;---

(show-paren-mode t)

(setq-default ediff-ignore-similar-regions t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 150)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))