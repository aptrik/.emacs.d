;;; aptrik-theme.el --- Light background theme.
;; (load-theme 'aptrik t)

(deftheme aptrik
  "My personal theme.")

(custom-theme-set-faces
 'aptrik

 ;; basic coloring
 '(default ((t (:foreground "black" :background "#f5f0ea"))))

 '(button ((t (:underline t))))
 '(fringe ((t (:background "#eaeaea"))))
 `(cursor ((t (:foreground "white" :background "firebrick" :inverse-video t))))
 `(highlight ((t (:background "DarkSeaGreen2"))))
 `(highlight-indent-face ((t (:background "honeydew2"))))
 `(lazy-highlight ((t (:foreground "black" :background "orange" :weight normal))))
 `(match ((t (:background "#FFFF96" :foreground "black" :weight bold))))
 `(mouse ((t (:foreground "white" :background "red" :inverse-video t))))
 `(region ((t (:foreground "black" :background "#b5d5ff"))))

 `(success ((t (:foreground "green"))))
 `(warning ((t (:foreground "yellow"))))
 `(error ((t (:foreground "orange"))))

 ;; comint
 `(comint-highlight-input ((t (:bold t))))
 `(comint-highlight-prompt ((t (:foreground "dark blue"))))

 ;; compilation
 `(compilation-column ((t (:foreground "yellow"))))
 `(compilation-enter-directory ((t (:foreground "green"))))
 `(compilation-error ((t (:foreground "red" :weight bold :underline t))))
 `(compilation-info ((t (:foreground "green4" :underline t :weight bold))))
 `(compilation-leave-directory ((t (:foreground "green"))))
 `(compilation-line ((t (:foreground "yellow"))))
 `(compilation-line-number ((t (:foreground "firebrick" :weight bold))))
 `(compilation-message ((t (:foreground "blue"))))
 `(compilation-warning ((t (:foreground "black" :background "yellow" :weight bold :underline t))))

 `(compilation-mode-line-exit
   ((t (:inherit compilation-info :foreground "white" :background "green4" :weight bold))))
 `(compilation-mode-line-fail
   ((t (:inherit compilation-error :foreground "indian red" :weight bold))))
 `(compilation-mode-line-run ((t (:foreground "orange2" :weight bold))))

 ;; diff
 `(diff-added ((t (:foreground "green4"))))
 `(diff-hl-change ((t (:background "SteelBlue2"))))
 `(diff-hl-delete ((t (:background "red2"))))
 `(diff-hl-insert ((t (:background "green3"))))
 `(diff-removed ((t (:foreground "red3"))))

 ;; dired
 `(dired-header ((t (:inherit font-lock-type-face :weight bold))))
 `(dired-directory ((t (:inherit font-lock-function-name-face))))
 `(dired-flagged ((t (:foreground "red" :weight bold))))
 `(dired-ignored ((t (:inherit shadow))))
 `(dired-mark ((t (:foreground "red" :weight bold))))
 `(dired-marked ((t (:foreground "orange" :weight bold))))
 '(dired-perm-write ((t (:forground "red" :underline t))))
 `(dired-symlink ((t (:foreground "dark blue" :weight bold))))
 `(dired-warning ((t (:foreground "orange" :underline t))))

 ;; font lock
 `(font-lock-builtin-face ((t (:foreground "Orchid"))))
 `(font-lock-comment-face ((t (:foreground "Firebrick"))))
 `(font-lock-constant-face ((t (:foreground "CadetBlue"))))
 `(font-lock-doc-face ((t (:inherit font-lock-string-face))))
 `(font-lock-function-name-face ((t (:foreground "blue"))))
 `(font-lock-keyword-face ((t (:foreground "black" :bold t))))
 `(font-lock-string-face ((t (:foreground "RosyBrown"))))
 `(font-lock-type-face ((t (:foreground "ForestGreen"))))
 `(font-lock-variable-name-face ((t (:foreground "DarkGoldenrod"))))
 `(font-lock-warning-face ((t (:bold t :foreground "Red1"))))

 ;; highlight-indentation
 `(highlight-indentation-face ((t (:background "#e3e3d3"))))
 `(highlight-indentation-current-column-face ((t (:background "#e3e3d3"))))

 ;; hl-line-mode
 `(hl-line ((t (:background "#B4EEB4"))))

 ;; hl-tags-mode
 `(hl-tags-face ((t (:inherit isearch))))

 ;; idle-highlight
 `(idle-highlight ((t (:inherit isearch))))

 ;; isearch
 `(isearch ((t (:foreground "black" :background "#FFCC99"))))
 `(isearch-lazy-highlight-face ((t (:foreground "black" :background "#FFFF96"))))
 `(isearch-fail ((t (:foreground "yellow" :background "red" :bold t))))

 ;; js2
 `(js2-error-face ((t (:background "indian red" :foreground "white" :bold t))))
 `(js2-warning-face ((t (:background "indian red" :foreground "white"))))

 ;; minibuffer
 `(minibuffer-prompt ((t (:foreground "medium blue"))))

 ;; mode-line
 `(mode-line ((((class color) (min-colors 88))
               (:foreground "black" :background "grey75" :box
                            (:line-width -1 :style released-button)))))
 `(mode-line-highlight
   ((((class color) (min-colors 88))
     (:box (:line-width 2 :color "grey40" :style released-button)))))
 `(mode-line-inactive ((default (:inherit mode-line))
                       (((class color)
                         (min-colors 88)
                         (background light))
                        (:foreground "grey20" :background "grey90" :box
                                     (:line-width -1 :color "grey75")
                                     :weight light))))

 ;; show-paren
 `(show-paren-match
   ((t (:foreground "black" :background "green" :bold t))))
 `(show-paren-mismatch
   ((t (:foreground "yellow" :background "indian red" :bold t))))

 ;; smartparens
 `(sp-pair-overlay-face ((t (:inherit show-paren-match))))
 `(sp-show-pair-match-face ((t (:inherit sp-pair-overlay-face))))
 `(sp-show-pair-mismatch-face ((t (:inherit show-paren-mismatch))))

 ;; web-mode
 `(web-mode-block-face ((t :background "#ffffe0")))
 `(web-mode-html-attr-name-face ((t (:foreground "DarkGoldenrod"))))
 `(web-mode-html-attr-value-face ((t (:foreground "RosyBrown"))))
 `(web-mode-html-tag-face ((t (:foreground "Blue1"))))
 `(web-mode-variable-name-face ((t (:inherit font-lock-variable-name-face))))
 `(web-mode-symbol-face ((t (:inherit font-lock-constant-face))))

 ;; nxml-mode
 `(nxml-element-local-name-face ((t (:inherit font-lock-function-name-face))))
 `(nxml-comment-content-face ((t (:slant normal :inherit font-lock-comment-face))))
 `(nxml-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 `(nxml-attribute-local-name-face ((t (:inherit font-lock-type-face))))
 `(nxml-attribute-value-face ((t (:inherit font-lock-string-face))))

 ;; magit
 `(magit-item-highlight ((t nil)))
 )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

(provide-theme 'aptrik)

;;; aptrik-theme.el ends here
