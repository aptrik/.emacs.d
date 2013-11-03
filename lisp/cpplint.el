;;; Code:

(defgroup cpplint nil
  "Minor mode for running cpplint."
  :prefix "cpplint-"
  :group 'tools)

(defvar cpplint-last-buffer nil
  "The most recent CPPLINT buffer.
A CPPLINT buffer becomes most recent when you select CPPLINT mode in it.
Notice that using \\[next-error] or \\[compile-goto-error] modifies
`complation-last-buffer' rather than `cpplint-last-buffer'.")

(defconst cpplint-regexp-alist
  ;; /users/berglund/work/forkservers/master/forkservers/common/MaveDriver.cc:0:  No copyright message found.  You should have a line: "Copyright [year] <Copyright Owner>"  [legal/copyright] [5]
  (list
   '("^\\(.*\\):\\([0-9]+\\):\s+\\(\\.*\\)$" 1 2 nil 1))
  "Regexp used to match CPPLINT hits.  See `compilation-error-regexp-alist'.")

(defcustom cpplint-options '("--verbose 1")
  "Options to pass to cpplint"
  :type '(repeat string)
  :group 'cpplint)

(defcustom cpplint-command "cpplint"
  "CPPLINT command."
  :type '(file)
  :group 'cpplint)

(defcustom cpplint-ask-about-save nil
  "Non-nil means \\[cpplint] asks which buffers to save before compiling.
Otherwise, it saves all modified buffers without asking."
  :type 'boolean
  :group 'cpplint)

(define-compilation-mode cpplint-mode "CPPLINT"
  (setq cpplint-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-error-regexp-alist)
       cpplint-regexp-alist)
  (set (make-local-variable 'compilation-disable-input) t))

(defvar cpplint-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map compilation-minor-mode-map)
    (define-key map " " 'scroll-up)
    (define-key map "\^?" 'scroll-down)
    (define-key map "\C-c\C-f" 'next-error-follow-minor-mode)

    (define-key map "\r" 'compile-goto-error)  ;; ?
    (define-key map "n" 'next-error-no-select)
    (define-key map "p" 'previous-error-no-select)
    (define-key map "{" 'compilation-previous-file)
    (define-key map "}" 'compilation-next-file)
    (define-key map "\t" 'compilation-next-error)
    (define-key map [backtab] 'compilation-previous-error)
    map)
  "Keymap for CPPLINT buffers.
`compilation-minor-mode-map' is a cdr of this.")

;;;###autoload
(defun cpplint ()
  "Run CPPLINT, and collect output in a buffer.
While cpplint runs asynchronously, you can use \\[next-error] (M-x next-error),
or \\<cpplint-mode-map>\\[compile-goto-error] in the grep \
output buffer, to go to the lines where cpplint found matches."
  (interactive)

  (save-some-buffers (not cpplint-ask-about-save) nil)
  (let* ((tramp (tramp-tramp-file-p (buffer-file-name)))
         (file (or (and tramp
                        (aref (tramp-dissect-file-name (buffer-file-name)) 3))
                   (buffer-file-name)))
         (command (mapconcat
                   'identity
                   (list cpplint-command
                         (mapconcat 'identity cpplint-options " ")
                         (format "'%s'" (comint-quote-filename file))
                         "|" "sort" "-n" "-t:" "-k2")
                   " ")))
    (compilation-start command 'cpplint-mode)))

(provide 'cpplint)
