;; See: http://code.google.com/p/js2-mode/

(setq js2-basic-offset 4
      js2-use-font-lock-faces t)

(add-hook 'js2-mode-hook 'setup--js2-mode)

(defun setup--js2-mode ()
  (require 'js-comint)
  (setq inferior-js-program-command
        "java org.mozilla.javascript.tools.shell.Main")
  (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
  (local-set-key (kbd "C-M-x")   'js-send-last-sexp-and-go)
  (local-set-key (kbd "C-c b")   'js-send-buffer)
  (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
  (local-set-key (kbd "C-c l")   'js-load-file-and-go)

  (local-set-key [f7]   'flymake-goto-next-error)
  (local-set-key [M-f7] 'flymake-goto-prev-error)
  (local-set-key [C-f7] 'flymake-start-syntax-check)
  (local-set-key [S-f7] 'flymake-mode))

;; After js2 has parsed a js file, we look for jslint globals decl
;; comment ("/* global Fred, _, Harry */") and add any symbols to a
;; buffer-local var of acceptable global vars.
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (let ((btext (buffer-substring-no-properties 1 (buffer-size))))
              (setq js2-additional-externs
                    (split-string
                     (if (string-match "/\\* *global \\(.*\\)\\*/" btext) (match-string-no-properties 1 btext) "")
                     "[ ,\t\n]+" t))
              )))

(eval-after-load "js2-mode"
  '(progn
     ;;(setq js2-skip-preprocessor-directives t)
     (setq-default js2-additional-externs
                   '("$" "unsafeWindow" "localStorage" "jQuery"
                     "setTimeout" "setInterval" "location" "console"))

     (require 'flymake)
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'"
                    flymake-jslint-init
                    flymake-simple-cleanup
                    flymake-get-real-file-name))
     (add-to-list 'flymake-err-line-patterns
                  '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
                    nil 1 2 3))
     ))

(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "rhino" (list (expand-file-name "bin/lib/jslint.js" user-emacs-directory)
                        local-file))))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (mark) (point)
     "python -c 'import sys; import json; json.dump(json.load(sys.stdin), sys.stdout, indent=2)'" (buffer-name) t)))
