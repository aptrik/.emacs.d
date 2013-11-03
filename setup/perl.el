(defalias 'perl-mode 'cperl-mode)

(fset 'perldoc 'cperl-perldoc)

(setq interpreter-mode-alist
      (append '(("perl"     . perl-mode)
                ("perl5"    . perl-mode)
                ("miniperl" . perl-mode))
              interpreter-mode-alist))

(setq perl-indent-level                2
      perl-continued-statement-offset  2
      perl-continued-brace-offset      0
      perl-brace-offset                0
      perl-brace-imaginary-offset      0
      perl-label-offset               -2)

(setq cperl-indent-level                2
      cperl-continued-statement-offset  2
      cperl-continued-brace-offset      0
      cperl-brace-offset                0
      cperl-brace-imaginary-offset      0
      cperl-label-offset               -2
      cperl-electric-keywords           t
      cperl-electric-parens             nil
      cperl-merge-trailing-else         nil
      cperl-under-as-char               t
      cperl-invalid-face                nil
      ;;cperl-highlight-variables-indiscriminately t
      )
