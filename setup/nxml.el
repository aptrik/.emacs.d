(setq nxml-auto-insert-xml-declaration-flag nil
      nxml-bind-meta-tab-to-complete-flag t
      nxml-child-indent 2
      nxml-slash-auto-complete-flag t
      nxml-syntax-highlight-flag t)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)

;; Maven pom files should be treated as xml files.
(add-hook 'sgml-mode-hook 'setup--hl-tags-mode)
(add-hook 'nxml-mode-hook 'setup--hl-tags-mode)

(defun setup--hl-tags-mode ()
  (hl-tags-mode 1))

(defun xml-pretty-print-region (start end)
  "Pretty format XML markup in region.
You need to have nxml-mode installed to do this."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region (point-min) (point-max)))))

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

;; To edit Docbook XML, do:
;; * M-x nxml-mode
;; * M-x rng-set-schema-file RET /carm/proj/flamenco/tools/share/docbook/docbook-5.0/rng/docbookxi.rnc RET
