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
       - ")))

(provide 'org-templates)