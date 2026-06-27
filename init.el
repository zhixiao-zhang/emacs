;; -*- lexical-binding: t; -*-
(define-advice use-package
    (:around (orig package &rest body) use-with-binary)
  (let ((executable (plist-get body :with)))
    (when executable
      (setq body (seq-difference body `(:with ,executable))))
    (if (or (not executable) (executable-find executable))
        (apply orig package body))))

(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(defun my/apply-font-settings (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (let* ((font-size 20)
             (font-weight 'semibold)
             (latin-font (cond ((eq system-type 'darwin) "SF Mono")
                               ((eq system-type 'gnu/linux) "SFMono Nerd Font Mono")
                               ((eq system-type 'windows-nt)
                                (cond ((member "SF Mono" (font-family-list)) "SF Mono")
                                      (t "Consolas")))
                               (t nil)))
             (cjk-font (or (cl-find-if (lambda (f) (member f (font-family-list)))
                                      '("LXGW WenKai" "LXGW WenKai Screen" "微软雅黑" "宋体"))
                           nil)))

        (when latin-font
          (set-face-attribute 'default nil
                              :font (font-spec :family latin-font
                                             :size font-size
                                             :weight font-weight)))

        (when cjk-font
          (let ((cjk-spec (font-spec :family cjk-font
                                   :size font-size
                                   :weight font-weight)))
            (dolist (charset '(kana han cjk-misc bopomofo))
              (set-fontset-font t charset cjk-spec))))))))

(add-hook 'after-make-frame-functions #'my/apply-font-settings)

(my/apply-font-settings (selected-frame))

(load "~/.emacs.d/light-pink-theme.el")
(load-theme 'light-pink t)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil
              tab-width 2)

(global-auto-revert-mode t)
(electric-pair-mode t)
(pixel-scroll-mode 1)

;; User-defined Keybindings
(defun open-init-file()
	(interactive)
	(find-file "~/.emacs.d/init.el"))

(defun my-delete-space-to-next-char ()
  "Delete all spaces and tabs at point until a non-whitespace character is found."
  (interactive)
  (while (looking-at "\\s-")
    (delete-char 1)))

(global-set-key (kbd "C-c i") 'open-init-file)
(global-set-key (kbd "C-c d") 'my-delete-space-to-next-char)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c C-r") 'open-reading-list)

(with-eval-after-load 'package
	(add-to-list 'package-archives
							 '("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
	(require 'use-package))

(setq use-package-always-ensure t)

(use-package expand-region
  :defer t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/mark-inside-pairs)
         ("C-M-=" . er/mark-inside-quotes)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("PATH" "CPATH" "LIBRARY_PATH")))

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles partial-completion)))))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'org-templates)

(use-package markdown-mode
  :defer t
  :init (setq markdown-enable-math t
              markdown-hide-urls t
              markdown-fontify-code-blocks-natively t)
  :config (set-face-underline 'markdown-line-break-face nil))

(use-package dashboard
  :if (display-graphic-p)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-init-info))
  (setq dashboard-banner-logo-title "你枉读诗书习经典，岂不知非礼勿能言。")
  (setq dashboard-startup-banner "~/.emacs.d/dlma.png"))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package magit
  :defer t)

(use-package valign
  :defer t)

(use-package olivetti
  :defer t)

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil)

(use-package org-roam
  :defer t
  :init
  (setq org-roam-directory "~/Documents/Notes/"
        org-roam-capture-templates
        '(("c" "computer science" plain "%?"
           :target
           (file+head "cs/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("s" "sociology" plain "%?"
           :target
           (file+head "sociology/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("e" "english" plain "%?"
           :target
           (file+head "english/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)          
          ("o" "misc" plain "%?"
           :target
           (file+head "misc/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)))
  :bind (("C-c b l" . org-roam-buffer-toggle)
         ("C-c b f" . org-roam-node-find)
         ("C-c b g" . org-roam-graph)
         ("C-c b i" . org-roam-node-insert))
  :config
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :after org-roam)

(use-package auctex
  :if (display-graphic-p)
  :with "xetex"
  :defer t
  :init
  (setq-default TeX-engine 'xetex)
  (setq TeX-check-TeX nil
        TeX-parse-self t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-view-program-list '(("Skim" "open -a Skim.app %o"))))

(setq custom-file (make-temp-file "custom.el"))
