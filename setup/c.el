(add-hook 'idl-mode-hook
          (lambda ()
            (require 'idl-font-lock)))

(eval-when-compile
  (require 'cc-defs))

(defvar pabe-c-basic-offset 2
  "*Amount of basic offset used used in C/C++ modes.")

;; (defun c-lineup-streamop (langelem)
;;   ;; lineup stream operators
;;   (save-excursion
;;     (let* ((relpos (cdr langelem))
;;            (curcol (progn (goto-char relpos)
;;                           (current-column))))
;;       (re-search-forward "<<\\|>>" (c-point 'eol) 'move)
;;       (goto-char (match-beginning 0))
;;       (- (current-column) curcol))))

(defconst pabe-c-style
  `((c-recognize-knr-p . nil)
    (c-enable-xemacs-performance-kludge-p . t) ; speed up indentation in XEmacs
    ;;(c-echo-syntactic-information-p . t)
    ;;(c-basic-offset . 2)
    (c-comment-only-line-offset 0 . 0)
    (c-tab-always-indent . t)
    (c-indent-comments-syntactically-p . nil)
    (comment-column . 40)

    (c-cleanup-list . (;;brace-else-brace
                       ;;brace-elseif-brace
                       ;;brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))

    (c-offsets-alist . ((arglist-intro         . c-lineup-arglist-intro-after-paren)
                        (arglist-close         . c-lineup-arglist)
                        (brace-list-open       . +)
                        (case-label            . +)
                        (func-decl-cont        . c-lineup-java-throws)
                        (inher-cont            . c-lineup-java-inher)
                        (inline-open           . 0)
                        (knr-argdecl           . 0)
                        (knr-argdecl-intro     . 0)
                        (stream-op             . c-lineup-streamop)
                        (label                 . *)
                        (statement-block-intro . +)
                        (statement-case-open   . +)
                        (statement-cont
                         .
                         (,(when (fboundp 'c-no-indent-after-java-annotations)
                             'c-no-indent-after-java-annotations)
                          ,(when (fboundp 'c-lineup-assignments)
                             'c-lineup-assignments)
                          +))
                        (substatement-label    . 0)
                        (substatement-open     . +)
                        ))
    )
  "pabe C/C++ programming style.")

(defun pabe-set-c-style ()
  "Set the current buffer's c-style."
  (interactive)
  (make-local-variable 'c-tab-always-indent)
  (setq c-tab-always-indent t)
  (c-add-style "pabe-c-style" pabe-c-style t)
  (setq c-basic-offset pabe-c-basic-offset))

(defun pabe-c-mode-hook ()
  (which-function-mode 1)
  (ignore-errors
    (c-subword-mode 1))
  (ignore-errors
    (subword-mode 1))

  (pabe-set-c-style)

  ;; (require 'doxymacs)
  ;; (doxymacs-mode t)
  ;; (doxymacs-font-lock)

  (set (make-local-variable 'compile-command)
       (concat "gmake -C " default-directory " all"))

  (c-toggle-electric-state 1)
  (c-toggle-auto-newline 1)
  ;; (c-toggle-hungry-state 1)
  (abbrev-mode 0)

  (local-set-key (kbd "C-c o") 'ff-find-other-file)
  (local-set-key (kbd "C-c c") 'compile)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'c-mode-hook 'pabe-c-mode-hook)
(add-hook 'c++-mode-hook 'pabe-c-mode-hook)

(eval-after-load "cc-mode"
  '(progn
     (require 'smart-snippet)
     (smart-snippet-with-abbrev-tables
      (c-mode-abbrev-table c++-mode-abbrev-table)
      ("namespace" (concat "namespace $${name} {\n"
                           "$.\n"
                           "} // namespace $${name}") 'bol?)
      ("class" (concat "$>class $${name}\n"
                       "$>{\n"
                       "$>public:\n"
                       "$>$${name}() {\n"
                       "$>$.\n"
                       "$>}\n"
                       "$>};\n") '(not in-comment?))
      ("if" (concat "$>if ($${condition}) {\n"
                    "$>$.\n"
                    "$>}") 'bol?)
      ("elsif" (concat "$>else if ($${condition}) {\n"
                       "$>$.\n"
                       "$>}") 'bol?)
      ("else" (concat "$>else {\n"
                      "$>$.\n"
                      "$>}") 'bol?)
      ("for" (concat "$>for ($${init}; $${cond}; $${step}) {\n"
                     "$>$.\n"
                     "$>}") 'bol?)
      )))
