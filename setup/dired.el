(unless (boundp 'dired-guess-shell-alist-user)
  (setq dired-guess-shell-alist-user '()))

(cond
 (linuxp
  (add-to-list 'dired-guess-shell-alist-user
               '("\\.\\(jpe?g\\|gif\\|png\\)\\'" "feh -drF --sort filename -D5 * &"))
  (add-to-list 'dired-guess-shell-alist-user
               '("\\.\\(avi\\|mpg\\|wmv\\|mp4\\|mov\\|m4v\\)\\'" "mplayer -really-quiet * &"))
  (add-to-list 'dired-guess-shell-alist-user
               '("\\.pdf\\'" "acroread * &" "evince * &"))
  (add-to-list 'dired-guess-shell-alist-user
               '("\\.epub\\'" "FBReader * &" "evince * &"))))

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you click on."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
        (goto-char (posn-point (event-end event)))
        (setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(defun dired-jump-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(setq dired-recursive-copies  'always
      dired-recursive-deletes 'top
      dired-dwim-target       t)

;; (setq dired-use-ls-dired nil)
;; (setq ls-lisp-use-insert-directory-program nil)
;; (require 'ls-lisp)

(setq dired-details-hidden-string     ""
      dired-details-initially-hide    nil
      dired-details-hide-link-targets nil
      wdired-allow-to-change-permissions t)

(require 'dired-details)
(dired-details-install)

(add-hook 'dired-load-hook 'setup--dired-load)

(defun setup--dired-load ()
  (require 'dired-x)
  (require 'dired-sort-menu+))

(defun dired-mark-torrent-files ()
  (interactive)
  (dired-mark-extension ".torrent"))

(add-hook 'dired-mode-hook 'setup--dired-mode)
;; (remove-hook 'dired-mode-hook 'setup--dired-mode)

(defun setup--dired-mode ()
  (setq dired-omit-files "^\\.[^.]")
  (if (< emacs-major-version 22)
      (setq dired-omit-files-p t)
    (dired-omit-mode 1))

  (setq truncate-lines t)

  (define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'dired-jump-to-top)
  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

  (local-set-key (kbd "M-o") 'dired-omit-mode)
  (local-set-key "%t"        'dired-mark-torrent-files)
  (local-set-key [mouse-2]   'dired-mouse-find-file)
  (local-set-key (kbd "T")   'dired-do-touch)
  )
