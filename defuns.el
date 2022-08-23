;; -*- lexical-binding: t -*-

(provide 'defuns)

;;-----------------------------------------------------------------------------
;;; Constants for the current environment.

(defconst linuxp
  (memq system-type '(gnu gnu/linux))
  "Are we running on a GNU/Linux system?")

(defconst macosp
  (eq system-type 'darwin)
  "Are we running on a Mac?")

(defvar unix-system-type
  (if (eq system-type 'darwin)
      'macos
    (let
        ((lsb-release (shell-command-to-string "lsb_release -ds")))
      (cond
       ((string-match "Red Hat Enterprise Linux Server release 6" lsb-release)
        'rhel6)
       ((string-match "Red Hat Enterprise Linux Server release 7" lsb-release)
        'rhel7)
       ((string-match "Fedora release" lsb-release)
        'fedora)
       (t
        nil))))
    "Which unix flavour are we running on?")

;;-----------------------------------------------------------------------------
;;; Turn simple modes on or off.

(defun turn-off-hl-line-mode ()
  (hl-line-mode 0))

(defun turn-on-rainbow-mode ()
  (rainbow-mode 1))

(defun turn-on-truncate-lines ()
  (toggle-truncate-lines 1))

(defun turn-on-hl-tags-mode ()
  (hl-tags-mode 1))

;;-----------------------------------------------------------------------------
;;; Change built-ins behaviours .

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

;;-----------------------------------------------------------------------------
;;; Miscellaneous routines

(defmacro xlaunch (&rest x)
  (list 'if (display-graphic-p)
        (cons 'progn x)))

(defun buffer-file-name-body ()
  "Buffer file name stripped of directory and extension."
  (if (buffer-file-name)
      (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
    (cadr (reverse (split-string (dired-current-directory) "/")))))

(defun comment-or-uncomment-current-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (and transient-mark-mode mark-active)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))

(defun copy-current-file-path ()
  "Add current file path to kill ring."
  (interactive)
  (kill-new (buffer-file-name)))

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (forward-line 1)
    (yank)
    (move-to-column cursor-column)))

(defun ediff-dired-marked-files ()
  "Run ediff on files in a dired buffer."
  (interactive)
  (let ((files (dired-get-marked-files))
        (wnd (current-window-configuration)))
    (if (<= (length files) 2)
        (let ((file1 (car files))
              (file2 (if (cdr files)
                         (cadr files)
                       (read-file-name
                        "file B: "
                        (dired-dwim-target-directory)))))
          (ediff-files file1 file2)
          (add-hook 'ediff-after-quit-hook-internal
                    (lambda ()
                      (defvar ediff-after-quit-hook-internal nil)
                      (set-window-configuration wnd))))
      (error "No more than 2 files can be marked."))))

(defun ediff-revision-current-buffer ()
  "Run ediff-revision on current buffer's file."
  (interactive)
  (if (not (buffer-file-name))
      (error "Current buffer is not visiting a file."))
  (if (and (buffer-modified-p)
           (y-or-n-p (message "Buffer %s is modified. Save buffer? " (buffer-name))))
      (save-buffer (current-buffer)))
  (require 'ediff-init)
  (require 'ediff-vers)
  (eval-when-compile
    (defvar ediff-version-control-package))
  (funcall (intern (format "ediff-%S-internal" ediff-version-control-package))
           "" "" nil))

(defun give-me-a-scratch-buffer-now (want-new)
  "Bring up *scratch* or younger siblings if prefixed."
  (interactive "P")
  (switch-to-buffer
   (if want-new
       (generate-new-buffer "*scratch*")
     "*scratch*"))
  (lisp-interaction-mode))

(fset 'scratch 'give-me-a-scratch-buffer-now)

(defun insert-path (file)
  "Insert a file path (with completion) at the current position."
  (interactive "FPath: ")
  (insert (expand-file-name file)))

(defun insert-org-header ()
  "Insert an org header at top of file."
  (interactive)
  (goto-char (point-min))
  (insert "-*- mode:org; coding:utf-8; ispell-local-dictionary:\"british\" -*-\n\n"))

(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (mark) (point)
     "python -c 'import sys; import json; json.dump(json.load(sys.stdin), sys.stdout, indent=2)'" (buffer-name) t)))

(defun lorem ()
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))

(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %.
Bound to `\\[match-paren]'."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(defun open-with ()
  "Open the underlying file of a buffer in an external program."
  (interactive)
  (when buffer-file-name
    (shell-command (concat
                    (if macosp
                        "open"
                      (read-shell-command
                       (format "Open current file (%s) with: " buffer-file-name)
                       ""
                       buffer-file-name))))))

(defun python-2to3-current-buffer ()
  "Convert current buffer from python 2 to python 3."
  (interactive)
  (save-excursion
    (when (buffer-modified-p)
      (save-buffer))
    (let* ((orig-buffer (current-buffer))
           (tmp-buffer (get-buffer-create "*2to3*"))
           (err-buffer (get-buffer-create "*2to3-error*")))
      (dolist (buf (list tmp-buffer err-buffer))
        (with-current-buffer buf
          (erase-buffer)))
      (shell-command (format "2to3 --no-diffs --nofix=future -w %s" (buffer-file-name orig-buffer))
                     tmp-buffer err-buffer)
      (mapc 'kill-buffer (list tmp-buffer err-buffer))
      (revert-buffer t t t))))

(defun sudo-edit (&optional arg)
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun sudo-edit-current-file ()
  "Reopen current file as root, preserving position."
  (interactive)
  (let ((p (point)))
    (find-alternate-file (concat "/sudo:root@localhost:"
                                 (buffer-file-name (current-buffer))))
    (goto-char p)))

(defun vc-examine (directory)
  (interactive
   (list
    (read-directory-name "VC examine (directory): "
                         nil default-directory nil)))
  (setq directory (file-name-as-directory (file-truename directory)))
  (cond
   ((or (file-readable-p (concat directory ".git"))
        (locate-dominating-file directory ".git"))
    (magit-status-setup-buffer (locate-dominating-file directory ".git")))
   ((or (file-readable-p (concat directory ".hg"))
        (locate-dominating-file directory ".hg"))
    (let ((default-directory directory))
      (ahg-status)))
   ((file-readable-p (concat directory "CVS"))
    (cvs-examine directory nil))
   (t
    (message "*** No version control system found for directory: %s" directory))))

(defun xml-pretty-print-region ()
  "Pretty format XML in region.
You need to have xmllint installed."
  (interactive)
  (save-excursion
    (save-restriction
      (let (start end)
        (if (use-region-p)
            (setq start (region-beginning)
                  end (region-end))
          (setq start (point-min)
                end (point-max)))
        (narrow-to-region start end)
        (shell-command-on-region
         start end
         "xmllint -format -"
         (current-buffer)
         t "*Xmllint Error Buffer*" t)))))

(defun xml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while (condition-case nil
                   (progn
                     (nxml-backward-up-element) ; always returns nil
                     t)
                 (error nil))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (message "/%s" (mapconcat 'identity path "/"))))))

;;-----------------------------------------------------------------------------
;;; Follow symbolic links when opening files.

;; Usage:
;;  (add-hook 'find-file-hook 'find-file--follow-symlink)

(defun find-file--follow-symlink ()
  "Function for `find-file-hook' that follow a symlink."
  (when buffer-file-name
    (let ((link-to (and (file-symlink-p buffer-file-name)
                        (file-chase-links buffer-file-name))))
      (if link-to
          (progn
            (buffer--follow-symlink)
            (message "Followed symlink to: %s" buffer-file-name))))))

(defun buffer--follow-symlink ()
  "If current buffer visits a symbolic link, visit the real file.
If the real file is already visited in another buffer, make that buffer
current, and kill the buffer that visits the link."
  (let* ((truename (abbreviate-file-name (file-chase-links buffer-file-name)))
         (true-buffer (find-buffer-visiting truename))
         (this-buffer (current-buffer)))
    (if (eq true-buffer this-buffer)
        (progn
          (kill-buffer this-buffer)
          ;; In principle, we could do something like set-visited-file-name.
          ;; However, it can't be exactly the same as set-visited-file-name.
          ;; I'm not going to work out the details right now. -- rms.
          (set-buffer (find-file-noselect truename)))
      (set-buffer true-buffer)
      (kill-buffer this-buffer))))

;;-----------------------------------------------------------------------------
;;; Web search helpers.

(defun web-search (prompt url-begin)
  "Do a web search on the active region or prompt for a string."
  (interactive)
  (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (read-string (concat prompt) (thing-at-point 'symbol) nil nil))))
    (when (and (stringp text) (> (length text) 0))
      (browse-url (concat url-begin (url-hexify-string text))))
    (deactivate-mark)))

(defun google ()
  "Do a Google search on the active region or prompt for a string."
  (interactive)
  (web-search "Google search: "
              "http://www.google.com/search?ie=utf-8&oe=utf-8&q="))

(defun imdb ()
  "Do an IMDB search on the active region or prompt for a string."
  (interactive)
  (web-search "IMDB search: "
              "http://www.imdb.com/Tsearch?"))

;;-----------------------------------------------------------------------------
;;; Helpers for camel and snake case transformations.

(defun replace-region-with (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun transform-region-to-lower-camel-case ()
  (interactive)
  (require 's)
  (replace-region-with 's-lower-camel-case))

(defun transform-region-to-upper-camel-case ()
  (interactive)
  (require 's)
  (replace-region-with 's-upper-camel-case))

(defun transform-region-to-snake-case ()
  (interactive)
  (require 's)
  (replace-region-with 's-snake-case))

(defun transform-region-to-dashed ()
  (interactive)
  (require 's)
  (replace-region-with 's-dashed-words))

;;-----------------------------------------------------------------------------
;;; Insert time and date stamps

(defvar pabe-rounding-minutes 5
  "Number of minutes to round time stamps to upon insertion.
When zero, insert the time unmodified.")

(defun pabe-round-time (time &optional mode)
  "Round a time to the closest `pabe-rounding-minutes' interval."
  (when (and
         (wholenump pabe-rounding-minutes)
         (/= 0 pabe-rounding-minutes))
    (let* ((t0       (float-time time))
           (rounding (* pabe-rounding-minutes 60))
           (delta    (mod t0 rounding)))
      (if (/= 0 delta)
          (let (t1)
            (cond
             ((and mode (< mode 0))
              (setq t1 (* (float (floor t0 rounding)) rounding)))
             ((and mode (> mode 0))
              (setq t1 (* (float (ceiling t0 rounding)) rounding)))
             (t
              (if (<= (- delta (/ rounding 2.0)) 0)
                  (setq t1 (* (float (floor t0 rounding)) rounding))
                (setq t1 (* (float (ceiling t0 rounding)) rounding)))))
            (list (truncate t1 65536.0)
                  (truncate (mod t1 65536.0))))
        time))))

(defun insert-date ()
  "Insert current date stamp according ISO 8601 format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d" (current-time))))

(defun insert-date-and-time ()
  "Insert current date and time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d  %H:%M--" (pabe-round-time (current-time) -1))))

(defun insert-time (&optional arg)
  "Insert current time stamp according ISO 8601 format."
  (interactive "p")
  (insert (format-time-string "%H:%M" (pabe-round-time (current-time) arg))))

(defun insert-timestamp ()
  "Insert current date and time as an ISO 8601 time stamp."
  (interactive)
  (insert (format-time-string "%Y-%m-%dT%H:%M" (current-time))))

(defun insert-week-number ()
  "Insert current ISO 8601 week number."
  (interactive)
  (insert (format-time-string "%Y-W%V" (current-time))))

(defun insert-change-signature (&optional arg)
  "Insert a changelog entry \"ISO-DATE USER-LOGIN-NAME\"\".
With a non-zero argument, insert \"ISO-DATE NAME  <E-MAIL>\".
Example:
  \\[insert-change-signature]     => 1999-02-19 <login-name>
  C-u \\[insert-change-signature] => 1999-02-19 <name> <email>"
  (interactive "P")
  (let ((today (format-time-string "%Y-%m-%d" (current-time))))
    (insert (if arg
                (format "%s %s <%s>"
                        today user-full-name user-mail-address)
              (format "%s %s"
                      today user-login-name)))))

;;-----------------------------------------------------------------------------
;;; Insert separator

(defvar separator-length 79
  "The length of the separator.")

(defun insert-separator (&optional end-col text sep)
  "Insert a separator, from current column to end-col.
Default end-column is equal to value of variable separator-length."
  (interactive "P")
  (let* ((pre  (separator-mode-modify-string
                (separator-clean-string
                 (if (stringp comment-start) comment-start "-- "))))
         (post (separator-mode-modify-string
                (separator-clean-string
                 (if (stringp comment-end) comment-end ""))))
         (mid  (separator-make-separator-string pre post end-col text sep))
         (len (+ (length pre) (length mid) (length post))))
    (if (= (+ (current-column)
              len)
           separator-length)
        (insert pre mid post)
      (insert pre " " mid post))
    (separator-cleanup-after)))

(defun insert-separator-dashed (&optional end-col)
  "Insert a dashed separator, from current column to end-col.
Default end-column is equal to value of variable separator-length."
  (interactive "P")
  (insert-separator end-col nil " "))

(defun insert-separator-text (&optional end-col)
  "Insert a separator-line with a text-string centered."
  (interactive "P")
  (insert-separator end-col (read-from-minibuffer "Text: ")))

(defun insert-separator-buffer-name (&optional end-col)
  "Insert a separator-line containing the the buffer-name centered."
  (interactive "P")
  (insert-separator end-col (buffer-name)))

(defun separator-make-separator-string (pre post &optional end-col text sep)
  ""
  (let* ((text (if (and (stringp text) (not (string-equal '"" text)))
                   (concat " " text " ")
                 ""))
         (len (max 5 (- (or end-col separator-length)
                        (length pre)
                        (length text)
                        (length post)
                        (current-column)))))
    (if sep
        (setq len (/ (1+ len) 2))
      (setq sep ""))
    (concat text (mapconcat 'identity (make-list len "-") sep))))

(defun separator-cleanup-after ()
  (if (looking-at "[ \\t]*\n")
      (progn
        (delete-horizontal-space)
        (forward-char))
    (insert "\n")))

(defun separator-mode-modify-string (str)
  (cond ((string-equal str ";")
         ";;")
        (t
         str)))

(defun separator-clean-string (str)
  (concat (delq ?  (string-to-list str))))

(defun string-to-list (string)
  "Convert STRING to a LIST of characters."
  (let ((len (length string))
        (i 0)
        val)
    (setq val (make-list len 0))
    (let ((l val))
      (while (< i len)
        (setcar l (aref string i))
        (setq l (cdr l) i (1+ i))))
    val))

;;-----------------------------------------------------------------------------
;;; Cursor movement

(defun this-line-to-top-of-window (&optional line)
  "Reposition line at point to top of window. With ARG, put point on line ARG.
   Negativ arguments counts from bottom.
Bound to `\\[this-line-to-top-of-window]'."
  (interactive "P")
  (recenter (if (null line) 0 (prefix-numeric-value line))))

(defun this-line-to-bottom-of-window ()
  "Reposition line at point to bottom of window.
Bound to `\\[this-line-to-bottom-of-window]'."
  (interactive)
  (this-line-to-top-of-window -1))

(defun scroll-down-in-place (n)
  "Scroll buffer down, but try to keep cursor on the same line in window.
Bound to `\\[scroll-down-in-place]'."
  (interactive "p")
  (forward-line (* -1 n))
  (scroll-down n))

(defun scroll-up-in-place (n)
  "Scroll buffer up, but try to keep cursor on the same line in window.
Bound to `\\[scroll-up-in-place]'."
  (interactive "p")
  (forward-line n)
  (scroll-up n))

(defun cursor-to-top-of-window ()
  "Reposition cursor to top of window.
Bound to `\\[cursor-to-top-of-window]'."
  (interactive)
  (goto-char (point-min-win)))

(defun cursor-to-bottom-of-window ()
  "Reposition cursor to bottom of window.
Bound to `\\[cursor-to-bottom-of-window]'."
  (interactive)
  (goto-char (point-max-win))
  (beginning-of-line))

(defun cursor-to-top-of-buffer ()
  "Reposition cursor to top of buffer.
Bound to `\\[cursor-to-top-of-buffer]'."
  (interactive)
  (goto-char (point-min)))

(defun cursor-to-bottom-of-buffer ()
  "Reposition cursor to bottom of buffer.
Bound to `\\[cursor-to-bottom-of-buffer]'."
  (interactive)
  (goto-char (point-max)))

(defun point-min-win ()
  "Return value of point when at top of current window."
  (save-excursion
    (move-to-window-line 0)
    (point)))

(defun point-max-win ()
  "Return value of point when at bottom of current window."
  (save-excursion
    (move-to-window-line -1)
    (end-of-line)
    (point)))

;;-----------------------------------------------------------------------------
;;; Buffer movement

(defun kill-junk-buffers ()
  "Kill some buffers that I don't like. :-)"
  (interactive)
  (mapcar (lambda (buf)
            (let ((name (buffer-name buf)))
              (cond ((eq ?\ (aref name 0))
                     buf)
                    ((string-equal "*scratch*" name)
                     buf)
                    ((string-equal "*Messages*" name)
                     buf)
                    ((eq ?* (aref name 0))
                     (kill-buffer buf))
                    (t
                     buf))))
          (buffer-list)))

(defun buffer-switch (&optional N)
  "Switch between the two most recent buffers.
Optionally replace current buffer with number N in the buffer list."
  (interactive "P")
  (or N (setq N 2))
  (let ((tail (buffer-list)) buf)
    (while (and tail (> N 0))
      (setq buf (car tail))
      (let ((first (aref (buffer-name buf) 0)))
        (if (not (or (= first ?\ )
                     ;;(= first ?*)
                     (string-equal (buffer-name buf)
                                   "*compilation*")))
            (setq N (- N 1))))
      (setq tail (cdr tail)))
    (if (bufferp buf)
        (switch-to-buffer buf)
      (message "cannot switch buffer to %s" buf))))

(defun unbury-buffer (&optional buf)
  "Select buffer BUF, or the last one in the buffer list.
This function is the opposite of `bury-buffer'."
  (interactive)
  (or buf (setq buf (car (reverse (buffer-list)))))
  (switch-to-buffer buf))

;;-----------------------------------------------------------------------------
;;; Shift region left and right

(defvar shift-offset 1
  "*Amount of offset per level of indentation.")

(defun shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun shift-region-left (start end &optional count)
  "Shift region of text to the left.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the left, by `shift-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, dedent only the current line.
You cannot dedent the region if any line is already at column zero."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (shift-region start end (- (prefix-numeric-value
                              (or count shift-offset)))))

(defun shift-region-right (start end &optional count)
  "Shift region of text to the right.
The lines from the line containing the start of the current region up
to (but not including) the line containing the end of the region are
shifted to the right, by `shift-offset' columns.

If a prefix argument is given, the region is instead shifted by that
many columns.  With no active region, indent only the current line."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (shift-region start end (prefix-numeric-value
                           (or count shift-offset))))

;;-----------------------------------------------------------------------------
;;; Cleanup functions.

(defun cleanup-buffer ()
  "Cleanup current buffer with regard to whitespace."
  (interactive)
   (save-excursion
     (undo-boundary)
     (untabify (point-min) (point-max))
     ;;(set-buffer-file-coding-system 'utf-8-unix t)
     ;;(set-buffer-file-coding-system 'utf-8)
     (delete-trailing-whitespace)))

;;-----------------------------------------------------------------------------
;;; Modifications to find-file-at-point.

(defadvice find-file-at-point
  (around goto-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (progn
                (goto-char (point-min))
                (forward-line (1- line))))))

;;-----------------------------------------------------------------------------

(defun uuid-create ()
  "Create a new UUID from system entropy.
See http://en.wikipedia.org/wiki/Universally_unique_identifier"
  (let ((s (sha1 (format "%s%s%s%s%s%s%s%s%s%s%s%s"
                         (random most-positive-fixnum)
                         (user-full-name)
                         user-mail-address
                         (user-uid)
                         (system-name)
                         (emacs-pid)
                         (current-time)
                         (emacs-uptime)
                         (garbage-collect)
                         (buffer-list)
                         (recent-keys)
                         (when (file-exists-p "/dev/urandom")
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (call-process "head" "/dev/urandom" (current-buffer) nil
                                           "-c" (number-to-string 32))
                             (substring (buffer-string) 0 32)))))))
    (format "%s-%s-%d%s-%d%s-%s" ;; 8-4-4-4-12
            (substring s 0 8)
            (substring s 8 12)
            5 (substring s 13 16)
            8 (substring s 17 20)
            (substring s 20 32))))

(defun uuid-insert ()
  "Insert an UUID at point."
  (interactive)
  (insert (uuid-create)))

;;-----------------------------------------------------------------------------

(defun what-face (pos)
  "Determine the face at point."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;-----------------------------------------------------------------------------

(defun set-transparency (onfocus notfocus)
  "Set the transparency for emacs. Values are from 0 - 100"
  (interactive "nOn Focus: \nnOn Unfocus: ")
  (set-frame-parameter (selected-frame)
                       'alpha (list onfocus notfocus)))

;;-----------------------------------------------------------------------------

(defvar saved-window-configuration nil)

(defun push-window-configuration ()
  (interactive)
  (push (current-window-configuration) saved-window-configuration))

(defun pop-window-configuration ()
  (interactive)
  (let ((config (pop saved-window-configuration)))
    (if config
        (set-window-configuration config)
      (if (> (length (window-list)) 1)
          (delete-window)
        (bury-buffer)))))

;;-----------------------------------------------------------------------------

(defvar hotspot-directories
  (list "~" "~/Development" "~/work" "~/projects" "~/lib" "~/priv")
  "List of hotspot directories.")

(defun hotspot-add-directories (directories)
  "Add directories to hotspots."
  (dolist (dir directories)
    (if (file-directory-p dir)
        (add-to-list 'hotspot-directories dir t))))

(defun hotspots ()
  "Open a hotspot."
  (interactive)
  (require 'helm-bookmark)
  (helm
   :sources
   `(((name . "Directory Hotspots")
      (candidates . ,(hotspot--generate-directories))
      (action . (("Open" . (lambda (d) (find-file d)))
                 ("Version control" . (lambda (d) (vc-examine d))))))
     helm-source-bookmarks)))

(defun hotspot--generate-directories ()
  (let ((result ()))
    (dolist (dir hotspot-directories)
      (ignore-errors
        (dolist (found (-select
                        (lambda (d)
                          (or (string-match "/\\.emacs.d$" d)
                              (not (string-match "/\\." d))))
                        (f-directories (substitute-in-file-name dir))))
          (push found result))))
          ;; (add-to-list 'result found t))))
    result))


;;-----------------------------------------------------------------------------
;;; Helpers to access remote systems.

(defvar remote-edit-user (user-login-name))
(defvar remote-edit-host "devbox")
(defvar remote-edit-path "/tmp")
(defvar remote-edit-user-history nil)
(defvar remote-edit-host-history nil)
(defvar remote-edit-path-history nil)

(defun remote-edit (&optional sudo)
  "Connect to remote host using ssh.

If sudo is non-nil, invoke sudo on remote host."
  (interactive "p")
  (let ((user (read-string "Remote user: " remote-edit-user 'remote-edit-user-history remote-edit-user))
        (host (read-string "Host: " remote-edit-host 'remote-edit-host-history remote-edit-host))
        (path (read-string "Path: " remote-edit-path 'remote-edit-path-history remote-edit-path)))
    (find-file
     (if sudo
         (format "/ssh:%s@%s|sudo:%s:%s" user host host path)
       (format "/ssh:%s@%s:%s" user host path)))))

;;-----------------------------------------------------------------------------
