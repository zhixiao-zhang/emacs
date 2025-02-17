(set-frame-font "IBM Plex Mono-18" t t)
(set-fontset-font
 t
 'han
 (cond
	((member "LXGW WenKai" (font-family-list)) "LXGW WenKai")))

(load "~/.emacs.d/light-pink-theme.el")
(load-theme 'light-pink t)



(setq auto-save-default nil
      make-backup-files nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      inhibit-startup-screen t)

(setq-default indent-tabs-mode nil
              tab-width 2)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

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

(global-set-key (kbd "<f2>") 'open-init-file)
(global-set-key (kbd "C-c d") 'my-delete-space-to-next-char)

(with-eval-after-load 'package
	(add-to-list 'package-archives
							 '("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
	(require 'use-package))

(defun use-package-with-executable (orig package &rest body)
	(let ((exec (plist-get body :with)))
		(when exec
			(setq body (seq-difference body (list :with exec))))
		(if (or (not exec) (executable-find exec))
				(apply orig package body))))

(advice-add 'use-package :around #'use-package-with-executable)

(setq use-package-always-ensure t)

(use-package minions
  :defer t
  :config
  (minions-mode 1))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :hook (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  (setq exec-path-from-shell-variables'("PATH" "MANPATH" "LDFLAGS" "SDKROOT" "CPPFLAGS" "HOMEBREW_PREFIX" "HOMEBREW_CELLAR" "HOMEBREW_REPOSITORY" "INFOPATH")))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-x b" . consult-buffer)
  :init
  (setq recentf-max-menu-items 10))

(use-package expand-region
  :defer t)

(use-package vertico
  :hook (after-init . vertico-mode)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :defer t
  :bind
  ("C-s" . consult-line))

(use-package treesit-auto
  :hook
  (prog-mode . global-treesit-auto-mode)
  :init
  (setq treesit-font-lock-level 4)
  :config
  (dolist (remap '((html-mode . mhtml-mode)))
          (add-to-list 'major-mode-remap-alist remap))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  (with-eval-after-load 'c++-ts-mode))

(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-mode . eldoc-box-hover-mode)))

(use-package eglot
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (let ((clangd-args '("clangd"
       "--background-index"
       "--clang-tidy"
       "--log=verbose"
       "--clang-tidy-checks=performance-*,bugprone-*"
       "--all-scopes-completion"
       "--completion-style=detailed"
       "--header-insertion=iwyu"
       "--pch-storage=disk")))
    (dolist (mode '(c-ts-mode c++-ts-mode))
             (add-to-list 'eglot-server-programs (cons mode clangd-args))))))

(use-package corfu
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
  :hook (prog-mode . yas-minor-mode))

(use-package cc-mode
  :hook ((c-ts-mode . (lambda ()
                         (add-to-list 'c-default-style '(c-ts-mode . "llvm.org"))))
         (c++-ts-mode . (lambda ()
                         (add-to-list 'c-default-style '(c++-ts-mode . "llvm.org")))))
  :config
  (add-to-list 'load-path "~/.emacs.d/lisp/")
  (require 'llvm))

(use-package eglot-booster
  :ensure nil
  :after eglot
  :config (eglot-booster-mode))

(use-package markdown-mode
  :defer t)

(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq evil-want-minibuffer nil)
  :hook (after-init . evil-mode)
  :config
  (evil-set-undo-system 'undo-redo)
  (evil-define-key 'normal global-map (kbd "s") nil)
  (evil-define-key 'normal global-map (kbd "RET") 'er/expand-region)
  (evil-define-key 'normal global-map (kbd "SPC h") 'windmove-left)
  (evil-define-key 'normal global-map (kbd "SPC j") 'windmove-down)
  (evil-define-key 'normal global-map (kbd "SPC k") 'windmove-up)
  (evil-define-key 'normal global-map (kbd "SPC l") 'windmove-right)
  (evil-define-key 'normal global-map (kbd "SPC fm") 'eglot-format))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package vterm
  :defer t
  :hook (vterm-mode . (lambda ()
                         (evil-local-mode -1)
                         (setq-local global-hl-line-mode nil)
                         (setq cursor-type 'bar))))

(setq initial-scratch-message
      ";; 你当像鸟飞往你的山！\n;; Flee as a bird to your mountain!\n\n")

(setq custom-file (make-temp-file "custom.el"))
