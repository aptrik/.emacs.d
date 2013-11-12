(setq interpreter-mode-alist
      (append '(("python"     . python-mode)
                ("python2.4"  . python-mode)
                ("python2.5"  . python-mode)
                ("python2.6"  . python-mode)
                ("python2.7"  . python-mode)
                ("python3.2"  . python-mode))
              interpreter-mode-alist))

;; (add-to-list 'tramp-remote-process-environment
;;              "PYTHONPATH=/path/where/emacs.py/is/located")

(setq jedi:complete-on-dot nil)

(eval-when-compile (require 'jedi nil t))

(defadvice py-execute-buffer (around python-keep-focus activate)
  "return focus to python code buffer"
  (save-excursion ad-do-it))

(eval-after-load "python-mode"
  '(progn
     (modify-syntax-entry ?\_ "_" python-mode-syntax-table)

     (setenv "PYTHONPATH" (concat (if (getenv "PYTHONPATH") "$PYTHONPATH:" "")
                                  (expand-file-name "bin/lib/python" user-emacs-directory))
             t)

     (require 'tramp) ;; needed by pep8 and pylint
     (require 'python-pylint)
     (require 'python-pep8)

     (defadvice pdb (before gud-query-cmdline activate)
       "Provide a better default command line when called interactively."
       (interactive
        (list (gud-query-cmdline 'pdb
                                 (file-name-nondirectory buffer-file-name)))))
     ))

(add-hook 'python-mode-hook 'setup--python-mode)
;; (remove-hook 'python-mode-hook 'setup--python-mode)

(defun setup--python-mode ()
  (which-function-mode 1)
  (subword-mode 1)

  ;;(setq py-python-command-args '( "-colors" "Linux"))

  (jedi:setup)

  (let ((map jedi-mode-map))
    (define-key map (kbd "<C-tab>") 'bs-show)
    (define-key map (kbd "<M-tab>") 'jedi:complete)
    (define-key map (kbd "M-.")     'jedi:goto-definition)
    (define-key map (kbd "C-.")     'jedi:complete)
    (define-key map (kbd "M-,")     'jedi:goto-definition-pop-marker))

  ;;(whitespace-mode 1)
  ;;(turn-on-eldoc-mode) ; doesn't work with python-mode from https://launchpad.net/python-mode

  (set (make-variable-buffer-local 'outline-regexp) "def\\|class ")
  (set (make-variable-buffer-local 'indent-tabs-mode) nil)

  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c C-c") 'recompile)

  (let ((map subword-mode-map))
    (define-key map [M-left]       'subword-backward)
    (define-key map [M-right]      'subword-forward)
    (define-key map [C-left]       'subword-backward)
    (define-key map [C-right]      'subword-forward)
    (define-key map [C-backspace]  'subword-backward-kill))

  (local-set-key [C-M-up]   'py-beginning-of-block)
  (local-set-key [C-M-down] 'py-end-of-def-or-class)

  (local-set-key [M-up]   'py-beginning-of-def-or-class)
  (local-set-key [M-down] 'py-end-of-def-or-class)

  (local-set-key [f7] 'python-pylint)
  (local-set-key [C-f7] 'python-pep8)

  (local-set-key [f9]   'py-run)
  (local-set-key [S-f9] 'pdb) ; defined in gud
  (local-set-key [C-f9] 'compile)
  (local-set-key [M-f9] 'recompile))

(add-hook 'py-shell-hook
          (lambda ()
            (require 'comint)
            (local-set-key [M-up]   'comint-previous-matching-input-from-input)
            (local-set-key [M-down] 'comint-next-matching-input-from-input)))

;; (defun py-goto-block-end ()
;;   (interactive)
;;   (py-mark-block nil 'just-move))

(defun py-run ()
  "Run python on the file in the current buffer."
  (interactive)
  (compile (format "python \"%s\"" (buffer-file-name))))
