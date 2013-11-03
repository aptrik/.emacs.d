(add-hook 'emacs-lisp-mode-hook 'setup--emacs-lisp)
;;(remove-hook 'emacs-lisp-mode-hook 'setup--emacs-lisp)
(add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)

(eval-after-load "emacs-lisp-mode"
  '(progn
     (font-lock-add-keywords
      'emacs-lisp-mode
      '(("(\\<\\(\\(?:ert-\\)?deftest\\)\\> +\\([^ ()]+\\)"
         (1 'font-lock-keyword-face)
         (2 'font-lock-function-name-face))))
     ))

(defun setup--emacs-lisp ()
  (eldoc-mode 1)
  (elisp-slime-nav-mode 1)
  (idle-highlight-mode 1)

  (require 'auto-complete)
  (auto-complete-mode 1)
  (setq ac-sources (append ac-emacs-lisp-sources ac-sources))
  (setq ac-omni-completion-sources '(("require\s+'" ac-source-emacs-lisp-features)))

  ;; (smartparens-mode 1)
  ;; ;;(sp-use-smartparens-bindings)
  ;; (local-set-key (kbd "M-<left>") 'sp-backward-sexp)
  ;; (local-set-key (kbd "M-<right>") 'sp-forward-sexp)
  ;; (local-set-key (kbd "M-<up>") 'sp-previous-sexp)
  ;; (local-set-key (kbd "M-<down>") 'sp-next-sexp)
  ;; (local-set-key (kbd "C-(") 'sp-forward-slurp-sexp)
  ;; (local-set-key (kbd "C-)") 'sp-backward-slurp-sexp)

  (local-set-key (kbd "C-<backspace>") 'backward-kill-sexp)
  (local-set-key (kbd "C-<delete>") 'kill-sexp)

  (local-set-key (kbd "C-.") 'auto-complete)
  (local-set-key [C-f9] (lambda () (interactive) (ert t)))
  )

(defun imenu-elisp-sections ()
  (setq imenu-prev-index-position-function nil)
  (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

(defvar ac-emacs-lisp-sources '(ac-source-symbols))
(defvar ac-emacs-lisp-features nil)
(defvar ac-source-emacs-lisp-features
  '((init
     .
     (lambda ()
       (unless ac-emacs-lisp-features
         (let ((suffix (concat (regexp-opt (find-library-suffixes) t) "\\'")))
           (setq
            ac-emacs-lisp-features
            (delq nil
                  (apply 'append
                         (mapcar (lambda (dir)
                                   (if (file-directory-p dir)
                                       (mapcar (lambda (file)
                                                 (if (string-match suffix file)
                                                     (substring file 0 (match-beginning 0))))
                                               (directory-files dir))))
                                 load-path))))))))
    (candidates . (lambda () (all-completions ac-prefix ac-emacs-lisp-features)))))
