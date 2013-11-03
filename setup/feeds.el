;; (setq inoreader-feeds nil)
;; (inoreader-load-opml "~/Dropbox/backup/InoReader Subscriptions - 2013-09-09T16:38.xml")
;; (length inoreader-feeds)
;; (length (opml-find-urls "~/Dropbox/backup/InoReader Subscriptions - 2013-09-09T16:38.xml"))

(defvar inoreader-feeds ()
  "List of InoReader feeds.")

(defun inoreader-load-opml (file)
  "Load feeds from an OPML file into `inoreader-feeds'."
  (interactive "fOPML file: ")
  (setf inoreader-feeds (delete-duplicates (opml-find-urls file) :test #'string=)))

(defun opml-find-urls (file)
  "Find URLs in OPML FILE."
  (require 'xml)
  (opml--find-urls (xml-parse-file (expand-file-name file))))

(defun opml--find-urls (xml)
  "Extract feeds from OPML data."
  (loop for (tag attr . content) in (remove-if-not #'listp xml)
        when (assoc 'xmlUrl attr)
        collect (cdr it)
        else append (opml--find-urls content)))

;;-----------------------------------------------------------------------------

(setq elfeed-db-directory "~/Dropbox/elfeed")

(setq elfeed-feeds
      '("http://www.smbc-comics.com/rss.php"
        "http://www.discgolfanswerman.com/1/feed"
        ))
