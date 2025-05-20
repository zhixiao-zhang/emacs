;; -*- lexical-binding: t; -*-
(when (display-graphic-p)
  (let* ((font-size 18)
         (latin-font (cond ((eq system-type 'darwin) "SF Mono")
                           ((eq system-type 'gnu/linux) "SFMono")
                           (t nil)))
         (cjk-font (when (member "LXGW WenKai" (font-family-list))
                     "LXGW WenKai")))
    (when latin-font
      (set-frame-font (format "%s-%d" latin-font font-size) t t))
    (when cjk-font
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font t charset (font-spec :family cjk-font :size font-size))))))

(load "~/.emacs.d/light-pink-theme.el")
(load-theme 'light-pink t)

(setq mode-line-space " "
      mode-line-modes '("ⓐ")
      mode-line-lighter '((corfu-mode . "ⓒ")
                          (diff-hl-mode . "ⓗ")
                          (eldoc-mode . "ⓔ")
                          (flymake-mode . "ⓜ")
                          (flyspell-mode . "ⓢ")
                          (visual-line-mode . "ⓥ")
                          (whitespace-mode . "ⓦ")
                          (yas-minor-mode . "ⓨ")))

(dolist (mapping mode-line-lighter)
  (let ((mode (intern (symbol-name (car mapping))))
        (lighter (cdr mapping)))
    (push `(,mode ,lighter) mode-line-modes)))

(defun mode-line-compose (left right)
  `(:eval (let* ((left (format-mode-line ',left))
                 (right (format-mode-line ',right))
                 (glue (- (window-pixel-width)
                          (string-pixel-width left)
                          (string-pixel-width right))))
            `(,(string-replace "%" "%%" left)
              ,(propertize " " 'display
                           `(space :ascent 76
                                   :height 1.4
                                   :width (,(max 0 glue))))
              ,(string-replace "%" "%%" right)))))

(setq-default mode-line-format (mode-line-compose
                                (list mode-line-space
                                      mode-line-mule-info
                                      mode-line-modified
                                      mode-line-space
                                      '(:propertize "%b" face bold))
                                (list '(:propertize mode-name face bold)
                                      mode-line-space
                                      mode-line-modes
                                      mode-line-space)))

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
(global-set-key (kbd "M-《") 'beginning-of-buffer)
(global-set-key (kbd "M-》") 'end-of-buffer)

(defun env-flush ()
  (let* ((shell (or (getenv "SHELL") "/bin/sh"))
         (command (format "%s -l -c 'env'" shell)))
    (with-temp-buffer
      (call-process-shell-command command nil t)
      (goto-char (point-min))
      (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
        (when-let* ((variable (match-string 1))
                    (value (match-string 2)))
          (when (string= variable "PATH")
            (setenv variable value)
            (setq exec-path (split-string value path-separator))))))))

(with-eval-after-load 'package
	(add-to-list 'package-archives
							 '("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
	(require 'use-package))

(with-eval-after-load 'use-package
  (env-flush))

(define-advice use-package
    (:around (orig package &rest body) use-with-binary)
  (let ((executable (plist-get body :with)))
    (when executable
      (setq body (seq-difference body `(:with ,executable))))
    (if (or (not executable) (executable-find executable))
        (apply orig package body))))

(setq use-package-always-ensure t)

(use-package expand-region
  :defer t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/mark-inside-pairs)
         ("C-M-=" . er/mark-inside-quotes)))

(use-package icomplete
  :config
  (icomplete-mode t))

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package treesit-auto
  :defer t
  :hook
  (prog-mode . global-treesit-auto-mode)
  :init
  (setq treesit-font-lock-level 4)
  :config
  (dolist (remap '((html-mode . mhtml-mode)))
    (add-to-list 'major-mode-remap-alist remap))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (with-eval-after-load 'c++-ts-mode))

(use-package eglot
  :defer t
  :hook ((c-ts-mode c++-ts-mode python-ts-mode) . eglot-ensure)
  :bind (("C-c m" . eglot-format)
         ("C-c r" . eglot-rename))
  :config
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pyright-langserver" "--stdio")))
  (with-eval-after-load 'eglot
    (let ((clangd-args '("clangd"
                         "--background-index"
                         "--clang-tidy"
                         "--clang-tidy-checks=performance-*,bugprone-*"
                         "--all-scopes-completion"
                         "--completion-style=detailed"
                         "--header-insertion=iwyu"
                         "--pch-storage=disk")))
      (dolist (mode '(c-ts-mode c++-ts-mode))
        (add-to-list 'eglot-server-programs (cons mode clangd-args))))))

(use-package eldoc
  :hook (after-init . global-eldoc-mode)
  :init (setq eldoc-echo-area-display-truncation-message nil
              eldoc-echo-area-use-multiline-p nil
              eldoc-echo-area-prefer-doc-buffer 'maybe))

(use-package corfu
  :defer t
  :hook (after-init . global-corfu-mode)
  :init (setq corfu-auto t
              corfu-cycle t
              corfu-quit-no-match 'separator
              corfu-preselect 'prompt)
  :config (add-hook 'eshell-mode (lambda ()
                                   (setq-local corfu-auto nil)))
  :bind (:map corfu-map
              ([tab] . corfu-next)
              ([backtab] . corfu-previous)
              ([return] . corfu-send)
              ([escape] . corfu-quit)))

(use-package yasnippet
  :defer t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'org-templates)

(use-package cc-mode
  :hook ((c-ts-mode . (lambda ()
                        (add-to-list 'c-default-style '(c-ts-mode . "llvm.org"))))
         (c++-ts-mode . (lambda ()
                          (add-to-list 'c-default-style '(c++-ts-mode . "llvm.org")))))
  :config
  (require 'llvm))

(use-package eglot-booster
  :ensure nil
  :after eglot
  :config (eglot-booster-mode))

(use-package markdown-mode
  :defer t
  :init (setq markdown-enable-math t
              markdown-hide-urls t
              markdown-fontify-code-blocks-natively t)
  :config (set-face-underline 'markdown-line-break-face nil))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-init-info))
  (setq dashboard-banner-logo-title "你枉读诗书习经典，岂不知非礼勿能言。")
  (setq dashboard-startup-banner "~/.emacs.d/dlma.png"))

(use-package magit
  :with "git"
  :defer t)

(use-package auctex
  :with "xetex"
  :defer t
  :init
  (setq-default TeX-engine 'xetex)
  (setq TeX-check-TeX nil
        TeX-parse-self t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-mode t
        TeX-view-program-list '(("Skim" "open -a Skim.app %o"))))

(use-package ox-hugo
  :pin melpa
  :after ox)

(use-package flymake
  :ensure nil
  :init (define-fringe-bitmap 'flymake-fringe-indicator
          (vector #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00011100
                  #b00111110
                  #b00111110
                  #b00111110
                  #b00011100
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000))
  :config (setq flymake-indicator-type 'fringes
                flymake-note-bitmap '(flymake-fringe-indicator compilation-info)
                flymake-warning-bitmap '(flymake-fringe-indicator compilation-warning)
                flymake-error-bitmap '(flymake-fringe-indicator compilation-error)))

(require 'ai-wrapper)

(setq custom-file (make-temp-file "custom.el"))
