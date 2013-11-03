(setq explicit-shell-file-name "bash"
      shell-file-name shell-file-name
      shell-command-switch "-c")

(setenv "SHELL" shell-file-name)
(setenv "ESHELL" shell-file-name)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term shell-file-name))
    (switch-to-buffer-other-window "*ansi-term*")))

(setq comint-input-ignoredups t
      comint-scroll-show-maximum-output t
      comint-prompt-read-only t)

(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m)
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
(add-hook 'comint-output-filter-functions 'comint-postoutput-scroll-to-bottom)

(add-hook 'comint-mode-hook
          (lambda ()
            (local-set-key [M-up]   'comint-previous-matching-input-from-input)
            (local-set-key [M-down] 'comint-next-matching-input-from-input)))

(setq ansi-color-names-vector
      ["black" "red3" "green3" "yellow3"
       "blue2" "magenta3" "cyan3" "white"])

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(add-hook 'term-mode-hook (lambda () (hl-line-mode 0)))
