(defun insert-org-header ()
  "Insert an org header at top of file."
  (interactive)
  (goto-char (point-min))
  (insert "-*- mode:org; coding:utf-8; mode:flyspell; ispell-local-dictionary:\"british\" -*-\n\n"))

(setq
 ;;org-directory              "~/org"
 ;;org-default-notes-file     "~/Dropbox/TODO"
 org-replace-disputed-keys  t
 org-reverse-note-order     t
 org-level-color-stars-only t
 org-hide-leading-stars     nil
 org-odd-levels-only        nil
 org-src-fontify-natively   t
 org-log-done               'time
 org-clock-idle-time        nil)

(when (require 'which-func)
  (add-to-list 'which-func-modes 'org-mode))

(add-hook 'org-mode-hook 'setup--org-mode)

(defun setup--org-mode ()
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil)))

  ;; Set program to use when opening PDF files.
  ;; (setq org-file-apps (delq (assoc "\\.pdf\\'" org-file-apps) org-file-apps))
  (if macosp
      (add-to-list 'org-file-apps '("\\.pdf\\'" . "open %s"))
    (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))
  ;; (assoc "\\.pdf\\'" org-file-apps)

  (global-set-key (kbd "M-m") 'org-capture)

  (local-set-key [M-up]          'outline-previous-visible-heading)
  (local-set-key [M-down]        'outline-next-visible-heading)
  (local-set-key [(control tab)] 'bs-show))

(setq org-capture-templates
      '(("t" "Task" entry
         (file+headline"~/Dropbox/TODO" "Incoming")
         "* TODO %^{Task}
SCHEDULED: %^t
%?
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00|8:00}
:END:" :empty-lines-after 2)
        ("m" "Music" item
         (file+headline "~/Dropbox/TODO" "Music")
         "- [ ] %^{Music}" :empty-lines-after 2)
        ("M" "Movie/TV show" item
         (file+headline "~/Dropbox/TODO" "Movie/TV")
         "- [ ] %^{Movie/TV show}" :empty-lines-after 2)
        ))
