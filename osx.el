;;; osx.el --- OSX related configuration -*- lexical-binding: t; -*-

(setq ns-alternate-modifier 'none
      ns-command-modifier 'meta
      ns-control-modifier 'control
      ns-function-modifier 'none
      ns-right-alternate-modifier 'super
      ns-right-command-modifier 'meta)

(when (display-graphic-p)
    (menu-bar-mode 1))

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(defun finder ()
  "Opens file directory in Finder."
  (interactive)
  (let ((file (buffer-file-name)))
    (if file
        (shell-command
         (format "%s %s" (executable-find "open") (file-name-directory file)))
      (error "Buffer is not attached to any file."))))

(setq insert-directory-program "gls")
