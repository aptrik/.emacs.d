(setq mail-personal-alias-file (expand-file-name "~/.mail-abbrevs")
      mail-from-style          'angles
      mail-yank-prefix         ">"
      mail-archive-file-name   (expand-file-name
                                (concat "~/Mail/Sent-Mail/sent-"
                                        (downcase
                                         (format-time-string
                                          "sent-%h-%Y" (current-time))))))

;; Mail abbreviations
;(load "mailabbrev")
(add-hook 'mail-setup-hook 'mail-abbrevs-setup)

;; Supercite
(add-hook 'mail-citation-hook 'sc-cite-original)
