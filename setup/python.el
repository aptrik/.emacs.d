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
     (modify-syntax-entry ?\_ "_" py-mode-syntax-table)

     (setenv "PYTHONPATH" (concat (if (getenv "PYTHONPATH") "$PYTHONPATH:" "")
                                  (expand-file-name "bin/lib/python" user-emacs-directory))
             t)

     (require 'tramp) ;; needed by pep8 and pylint
     (require 'python-pylint)
     (require 'python-pep8)

     (require 'snippet)
     (define-abbrev-table 'python-mode-abbrev-table ())
     (snippet-with-abbrev-table
      'python-mode-abbrev-table
      ("class"    . "class $${name}(object):\n")
      ("def"      . "def $${name}($${self}$.):\n$>pass\n")
      ("error"    . "class $${name}(Exception): pass\n")
      ("init"     . "def __init__(self$.):\n$>pass\n")
      ("main"     . "if __name__ == '__main__':\n$>pass\n")
      ("prg"      . "os.path.basename(__file__)")
      ("super"    . "super($${class}, $${self}).$${method}()")
      ("testcase" . "class Test$${name}(unittest.TestCase):\n$>")
      ("testmain" . "if __name__ == '__main__':\n$>import unittest\n$>import sys\n$>unittest.TestProgram(argv=[sys.argv[0],\n$># '-v',\n$>#  'TestClass.testMethod',\n$>])\n")

      ("pylint" . "# pylint: disable=$${msg}")
      ("nocov" . "# pragma: no cover")
      ("pdb" . "import pdb\n$>pdb.set_trace()\n")
      ("pprint" . "$>import pprint\n$>pprint.pprint($.)\n")
      ("timeit" . "$>import timeit\n$>$${t} = timeit.Timer(\"$${f()}$.\", \"from __main__ import $${f()}\")\n$>print $${t}.timeit($${10})\n")
      ("time" . "$>import time\n$>$${t0} = time.time()\n$>r = $${42}$.\n$>$${t1} = time.time()\n$>print \"+++\", r, $${t1} - $${t0}\n")
      )

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

  (ignore-errors (c-subword-mode 1))
  (ignore-errors (subword-mode 1))

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
