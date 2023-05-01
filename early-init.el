;;; early-init.el -*- lexical-binding: t; -*-

;; (setq package-enable-at-startup nil)

(set-language-environment "UTF-8")
(setq default-input-method nil)

(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))
