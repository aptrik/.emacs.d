(defun reload-firefox ()
  "Reload Mozilla/Firefox; requires MozRepl addon."
  (interactive)
  (require 'moz)
  (comint-send-string (inferior-moz-process)
                      "setTimeout(BrowserReload(), \"1000\");"))

(defun auto-reload-firefox-after-save ()
  "Automatically reload Mozilla/Firefox after each save; requires MozRepl addon."
  (interactive)
  (add-hook 'after-save-hook
            (lambda ()
              (interactive)
              (comint-send-string (inferior-moz-process)
                                  "setTimeout(BrowserReload(), \"1000\");"))
            'append 'local)) ; buffer-local

;; (add-hook 'html-mode-hook 'auto-reload-firefox-on-after-save-hook)
;; (add-hook 'css-mode-hook 'auto-reload-firefox-on-after-save-hook)

(defun moz-reload-browser (&optional timeout)
  (interactive)
  (if timeout
    (comint-send-string (inferior-moz-process)
                        "setTimeout(BrowserReload(), \"1000\");")
    (comint-send-string (inferior-moz-process)
                        "BrowserReload();")))

(defun moz-enable-save-hook ()
  "Enable automatic mozilla reload.

Requires that the moz-minor-mode is enabled and that MozRepl is
running in Mozilla/Firefox.
"
  (interactive)
  (add-hook 'after-save-hook
            'moz-reload-browser
            'append 'local))

(defun moz-disable-save-hook ()
  "Disable automatic mozilla reload.

Requires that the moz-minor-mode is enabled and that MozRepl is
running in Mozilla/Firefox.
"
  (interactive)
  (remove-hook 'after-save-hook
               'moz-reload-browser
               'local))
