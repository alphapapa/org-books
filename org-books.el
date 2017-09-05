(require 'cl-lib)
(require 'org)

(require 'dash)
(require 'esxml-query)

(defun org-books--amazon (url)
  "Return plist of data for book at Amazon URL."
  (cl-flet ((field (target-field list)
                   (cl-loop for li in list
                            for (field value) = (ignore-errors
                                                  (-let (((_ _ (_ _ field) value) li))
                                                    (list field value)))
                            when (equal field target-field)
                            return (s-trim value))))
    (let* ((html (org-web-tools--get-url url))
           (tree (with-temp-buffer
                   (insert html)
                   (libxml-parse-html-region (point-min) (point-max))))
           (author (esxml-query "span.author a.contributorNameID *" tree))
           (title (esxml-query "div#booksTitle h1#title > span *" tree))
           (details (esxml-query-all "table#productDetailsTable ul li" tree))
           (date (s-replace "– " ""
                            (or
                             ;; Printed book
                             (third (esxml-query-all "div#booksTitle h1#title span *" tree))
                             ;; Kindle
                             (field "Publication Date:" details))))
           (asin (field "ASIN:" details))
           (publisher (-some->> (field "Publisher:" details)
                                (replace-regexp-in-string (rx " (" (1+ anything) ")") "")))
           (isbn-10 (field "ISBN-10:" details))
           (isbn-13 (field "ISBN-13:" details)))
      (list :author author :title title :publisher publisher :date date
            :asin asin :isbn-10 isbn-10 :isbn-13 isbn-13))))

(defun org-books-insert-entry (&optional url)
  "Insert Org entry for book at URL."
  (interactive)
  (cl-flet ((key-name (key)
                      (->> key
                        symbol-name
                        (replace-regexp-in-string "^:" "")
                        upcase)))
    (when-let ((url (or url (org-web-tools--get-first-url)))
               (data (org-books--amazon url))
               (property-drawer (list 'property-drawer nil
                                      (cl-loop for (key value) on data by #'cddr
                                               for key = (key-name key)
                                               when value
                                               collect (list 'node-property (list :key key :value value)))))
               (author (plist-get data :author))
               (title (plist-get data :title))
               (link (org-make-link-string url title))
               (heading (format "%s, %s" author link))
               (element (org-element-create 'headline (list :level 1 :title heading)
                                            property-drawer))
               (string (org-element-interpret-data element)))
      (kill-new string)
      (save-excursion
        (beginning-of-line)
        (org-paste-subtree)))))

(provide 'org-books)
