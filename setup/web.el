;;-----------------------------------------------------------------------------
;;; web-mode

(setq web-mode-enable-block-face t
      web-mode-enable-part-face t
      web-mode-disable-css-colorization nil
      web-mode-disable-auto-pairing nil
      )

;;-----------------------------------------------------------------------------
;;; HTML

(defun nxhtml-load ()
  (interactive)
  (require 'nxhtml-autostart "lisp/nxhtml/autostart"))

(setq nxhtml-global-minor-mode t
      mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil
      ;;nxml-degraded t
      )

(defun html-replace-string-pairs-region (start end mylist)
  "Replace string pairs in region."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (mapc
       (lambda (arg)
         (goto-char (point-min))
         (let ((case-fold-search nil))
           (while (search-forward (car arg) nil t)
             (replace-match (cadr arg) t t))))
       mylist))))

(defun convert-to-html-entities (start end)
  "Replace special characters with corresponding HTML entities."
  (interactive "r")
  (html-replace-string-pairs-region
   start end
   '(("å" "&aring;")
     ("ä" "&auml;")
     ("ö" "&ouml;")
     ("Å" "&Aring;")
     ("Ä" "&Auml;")
     ("Ö" "&Ouml;")
     ("é" "&eacute;")
     ("É" "&Eacute;")
     ("è" "&egrave;")
     ("È" "&Egrave;")
     ("ü" "&uuml;")
     ("Ü" "&Uuml;")
     )))

(defun convert-bad-utf8 (start end)
  "Convert badly encoded UTF-8 strings.
Also see/use `recode-region'."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (let ((case-replace nil)
            (chars '(("é" . "Ã©")
                     ("É" . "Ã")
                     ("è" . "Ã¨")
                     ("È" . "Ã")
                     ("ü" . "Ã¼")
                     ("Ü" . "Ã")
                     ("å" . "Ã¥")
                     ("ä" . "Ã¤")
                     ("ö" . "Ã¶")
                     ("Å" . "Ã…")
                     ("Ä" . "Ã")
                     ("Ö" . "Ã–")
                     )))
        (mapcar (lambda (p) (beginning-of-buffer)
                  (replace-string (cdr p) (car p) nil))
                chars)))))

;;-----------------------------------------------------------------------------
;;; CSS / SCSS / ...

(setq cssm-indent-function 'cssm-c-style-indenter
      css-indent-level 2)


(add-hook 'css-mode-hook 'setup--rainbow-mode)

(setq scss-compile-at-save nil)

;;-----------------------------------------------------------------------------
;;; PHP

(add-hook 'php-mode-hook 'setup--php-mode)

(setq php-extra-constants '())

(defun setup--php-mode()
  "PEAR/PHP setup."
  (setq case-fold-search t)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)

  (subword-mode 1)

  (setq c-electric-flag nil)
  (setq c-basic-offset 4)

  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close '0)
  (c-set-offset 'case-label 2)

  (setq php-warned-bad-indent t)

  (local-set-key [C-f7] 'php-lint)
  (local-set-key [f7] 'phpcs))

(defun php-lint ()
  "Performs a PHP lint check on the current file."
  (interactive)
  (let ((compilation-error-regexp-alist '(php))
        (compilation-error-regexp-alist-alist ()))
    (pushnew '(php "\\(syntax error.*\\) in \\(.*\\) on line \\([0-9]+\\)$" 2 3 nil nil 1)
             compilation-error-regexp-alist-alist)
    (compile (concat "php -l -f \"" (buffer-file-name) "\""))))

(defun phpcs ()
  "Performs a PHP code sniffer check on the current file."
  (interactive)
  (let ((compilation-error-regexp-alist '(php))
        (compilation-error-regexp-alist-alist ()))
    (pushnew '(php "\"\\([^\"]+\\)\",\\([0-9]+\\),\\([0-9]+\\),\\(warning\\|error\\),\\(.*\\)" 1 2 3 (5 . 6) 4)
    ;; (pushnew '(php "\"\\([^\"]+\\)\",\\([0-9]+\\),\\([0-9]+\\),\\(\\(warning\\)\\|\\(error\\)\\),\\(.*\\)" 1 2 3 (5 . 6) 4)
             compilation-error-regexp-alist-alist)
    ;; See:
    ;; * http://pear.php.net/manual/en/standards.php
    ;; * http://pear.php.net/manual/en/package.php.php-codesniffer.annotated-ruleset.php
    (compile (concat "phpcs"
                     " --standard=Zend"
                     " --report=csv"
                     " \"" (buffer-file-name) "\""))))

;;-----------------------------------------------------------------------------
