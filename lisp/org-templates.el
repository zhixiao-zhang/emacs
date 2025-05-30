;; -*- lexical-binding: t; -*-
(defun my-org-log-file ()
  "返回当前年月对应的log文件路径，如果文件不存在则创建"
  (let* ((year (format-time-string "%Y"))
         (month (format-time-string "%Y-%m"))
         (dir (expand-file-name (concat "~/Documents/log/" year "/")))
         (file (concat dir month ".org")))
    (unless (file-exists-p dir)
      (make-directory dir t))
    (unless (file-exists-p file)
      (write-region (format "#+TITLE: %s Log\n\n" month) nil file))
    (find-file file)))

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
        ("l" "log" entry
         (function my-org-log-file)
         "** %<%m-%d>
%?")
        ("t" "todo-list" )))

(provide 'org-templates)
