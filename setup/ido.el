(ido-mode 0)

(setq ido-default-buffer-method 'selected-window)

(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)

(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (or (nth 0 ta) 0) (or (nth 0 tb) 0))
                      (> (or (nth 1 ta) 0) (or (nth 1 tb)))
                    (> (or (nth 0 ta) 0) (or (nth 0 tb) 0)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))
