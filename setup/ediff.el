(eval-after-load "ediff"
  '(progn
     ;; ignore whitespaces and newlines
     (setq-default ediff-ignore-similar-regions t)

     ;; do not create new frame for the control panel
     (setq ediff-window-setup-function 'ediff-setup-windows-plain)

     (setq ediff-split-window-function (lambda (&optional arg)
                                         (if (> (frame-width) 150)
                                             (split-window-horizontally arg)
                                           (split-window-vertically arg))))))

(add-hook 'ediff-load-hook 'setup--ediff-load)

(defun setup--ediff-load ()
  (add-hook 'ediff-before-setup-hook
            (lambda ()
              (setq ediff-saved-window-configuration (current-window-configuration))))

  (let ((restore-window-configuration
         (lambda ()
           (set-window-configuration ediff-saved-window-configuration))))
    (add-hook 'ediff-quit-hook restore-window-configuration 'append)
    (add-hook 'ediff-suspend-hook restore-window-configuration 'append)))

(defun ediff-revision-current-buffer ()
  "Run ediff-revision on current buffer's file."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (error "Current buffer is not visiting a file."))))
    (if (and (buffer-modified-p)
             (y-or-n-p (message "Buffer %s is modified. Save buffer? " (buffer-name))))
        (save-buffer (current-buffer)))
    (require 'ediff-init)
    (require 'ediff-vers)
    (funcall (intern (format "ediff-%S-internal" ediff-version-control-package))
             "" "" nil)))

(eval-after-load 'vc
  '(progn
     (define-key vc-prefix-map "e" 'ediff-revision-current-buffer)))
