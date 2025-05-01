;; -*- lexical-binding: t; -*-
;; If you are distributing this theme, please replace this comment
;; with the appropriate license attributing the original VS Code
;; theme author.

(deftheme light-pink "A nice light theme.")


(let (
(color0 "#f5f5f5")
(color1 "#ede8fd")
(color3 "#D6D1E8")
(color6 "#ff7ab3")
(color7 "#aaaaaa")
(color8 "#9D3C5E")
(color9 "#BA9AB9")
(color10 "#B08B35")
(color11 "#E17092")
(color12 "#1F6E89")
(color13 "#9466AA")
(color14 "#736A6D")
(color16 "#eaeaea")
(color17 "#c46b81"))


(custom-theme-set-faces
'light-pink


;; BASIC FACES
`(default ((t (:background ,color0 ))))
`(hl-line ((t (:background ,color1 ))))
`(cursor ((t (:background ,color7 ))))
`(region ((t (:background ,color3 ))))
`(fringe ((t (:background ,color0 ))))
`(mode-line-inactive ((t (:foreground, color9 :background ,color0 ))))
`(mode-line ((t (:foreground, color6 :background ,color0 ))))
`(vertical-border ((t (:foreground ,color7 ))))


;; FONT LOCK FACES
`(font-lock-builtin-face ((t (:foreground ,color11 :weight bold ))))
`(font-lock-comment-face ((t (:foreground ,color9 :slant italic ))))
`(font-lock-constant-face ((t (:foreground ,color13 ))))
`(font-lock-number-face ((t (:foreground ,color10 ))))
`(font-lock-function-name-face ((t (:foreground ,color8 :weight bold ))))
`(font-lock-keyword-face ((t (:foreground ,color11 ))))
`(font-lock-string-face ((t (:foreground ,color12 ))))
`(font-lock-variable-name-face ((t (:foreground ,color13 ))))
`(font-lock-type-face ((t (:foreground, color13 :weight bold ))))
`(font-lock-operator-face ((t (:foreground, color7))))

;; display-line-number-mode
`(line-number ((t (:foreground ,color14 ))))
`(line-number-current-line ((t (:foreground ,color17 ))))


;; THIRD PARTY PACKAGE FACES

;; web-mode
`(web-mode-string-face ((t (:foreground ,color12 ))))
`(web-mode-html-tag-face ((t (:foreground ,color13 ))))
`(web-mode-html-tag-bracket-face ((t (:foreground ,color17 ))))


;; org-mode
`(org-block ((t (:background ,color16 ))))
`(org-block-begin-line ((t (:foreground ,color9 ))))))


(custom-theme-set-variables
  'light-pink
  '(linum-format " %3i "))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


;;;###autoload
(defun light-pink-theme()
  "Apply the light-pink-theme."
  (interactive)
  (load-theme 'light-pink t))


(provide-theme 'light-pink)


;; Local Variables:
;; no-byte-compile: t
;; End:


;; Generated using https://github.com/nice/themeforge
;; Feel free to remove the above URL and this line.
