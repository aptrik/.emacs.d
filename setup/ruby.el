(setq interpreter-mode-alist
      (append '(("ruby" . ruby-mode)
                ("ruby18" . ruby-mode))
              interpreter-mode-alist))

(add-to-list 'completion-ignored-extensions ".rbc")

(add-hook 'ruby-mode-hook 'setup--ruby-mode)
;; (remove-hook 'ruby-mode-hook 'setup--ruby-mode)

(eval-after-load "ruby-mode"
  '(progn
     (require 'ruby-end)
     (require 'smartparens-ruby)))

(defun setup--ruby-mode ()
  (rvm-activate-corresponding-ruby)
  (robe-mode 1)
  (which-function-mode 1)
  (show-smartparens-mode 1)

  (make-variable-buffer-local 'compilation-error-regexp-alist)
  (setq compilation-error-regexp-alist
        (append compilation-error-regexp-alist
                (list (list
                       (concat
                        "\\(.*?\\)\\([0-9A-Za-z_./\:-]+\\.rb\\)"
                        ":\\([0-9]+\\)") 2 3))))

  (make-variable-buffer-local 'compile-command)
  (setq compile-command (concat "ruby " (buffer-file-name) " "))

  ;; Hide-show
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def \\|class\\|module\\|do\\)" "end" "#"
                 (lambda (arg) (ruby-end-of-block)) nil))

  ;; Alignment
  (require 'align)
  (add-to-list 'align-rules-list
               '(ruby-comma-delimiter
                 (regexp . ",\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-hash-literal
                 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))

  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c C-c") 'recompile)

  (local-set-key [f9]    'ruby-run)
  (local-set-key [C-f9]  'ruby-test)
  (local-set-key [M-f9]  'ri))

(defun ruby-run ()
  "Run ruby on the file in the current buffer."
  (interactive)
  (compile (concat "ruby " (buffer-file-name))))

(defun ruby-test ()
  "Run testrb on the file in the current buffer."
  (interactive)
  (compile (concat "testrb " (file-name-directory (buffer-file-name)))))

(defun ruby-eval-buffer ()
  "Evaluate the buffer with ruby."
  (interactive)
  (shell-command-on-region (point-min) (point-max) "ruby"))
