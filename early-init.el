;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      package-enable-at-startup t
      package-native-compile t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

(provide 'early-init)
;;; early-init.el ends here
