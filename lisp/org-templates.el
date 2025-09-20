;; -*- lexical-binding: t; -*-
(defun zz/slugify (s)
  (let* ((down (downcase s))
         (s1 (replace-regexp-in-string "[^a-z0-9]+" "-" down)))
    (replace-regexp-in-string "^-\\|-$" "" s1)))

(defun zz/new-essay-path (title)
  (let* ((root (expand-file-name "~/src/emacs-based-site/blog/"))
         (year (format-time-string "%Y"))
         (dir  (expand-file-name year root))
         (slug (zz/slugify title))
         (fname (format "%s.org" slug))
         (path (expand-file-name fname dir)))
    (make-directory dir t)
    (expand-file-name fname dir)))

(defun zz/capture-essay-target ()
  (let* ((fname (read-string "File Name: "))
         (title (read-string "Title: "))
         (path  (zz/new-essay-path fname))
         (new   (not (file-exists-p fname))))
    (set-buffer (find-file path))
    (when new
      (insert (format "#+TITLE: %s\n#+DATE: %s\n\n\n"
                      title (format-time-string "%Y-%m-%d %H:%M"))))
    (org-capture-put :title title)
    (goto-char (point-max))))


(setq org-capture-templates
      '(("m" "meeting" entry
         (file+headline "~/Documents/meetings.org" "Meeting Notes")
         "* Meeting %<%Y-%m-%d>
   :PROPERTIES:
   :DATE: %<%Y-%m-%d %H:%M>
   :END:
   
   - *Topic*：%^{Meeting Topic}
   - *Members*：%^{Member}
   *** Notes
       - %?
   *** Summary
       - ")
        ("n" "New Essay" plain
         (function zz/capture-essay-target)
         "%?\n"
         :immediate-finish t
         :jump-to-captured t
         :unnarrowed t)))        

(provide 'org-templates)
