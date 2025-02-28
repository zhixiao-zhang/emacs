(set-frame-font "SF Mono-18" t t)
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
      inhibit-startup-screen t
      display-line-numbers-type 'relative)

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

(global-set-key (kbd "C-c i") 'open-init-file)
(global-set-key (kbd "C-c d") 'my-delete-space-to-next-char)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c j") 'windmove-down)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)

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

(use-package expand-region
  :defer t
  :bind (("C-=" . er/expand-region)
         ("M-=" . er/mark-inside-pairs)
         ("C-M-=" . er/mark-inside-quotes)))

(use-package vertico
  :defer t
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
  :defer t
  :init
  (setq completion-styles '(orderless)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :defer t
  :hook (after-init . marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

(use-package consult
  :defer t
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer))

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

(use-package eldoc-box
  :defer t
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-mode . eldoc-box-hover-mode))
  :config
  (set-face-attribute 'eldoc-box-body nil
                      :foreground "#000000"
                      :background "#FEF3C7"))

(use-package eglot
  :defer t
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)
  :bind ("C-c m" . eglot-format)
  :config
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

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-init-info))
  (setq dashboard-banner-logo-title "你当像鸟飞往你的山")
  (setq dashboard-startup-banner "~/Pictures/dont-leave-me-alone.png"))

(use-package vterm
  :defer t
  :hook (vterm-mode . (lambda ()
                         (setq-local global-hl-line-mode nil)
                         (setq cursor-type 'bar))))

(defvar vterm-compile-buffer nil)
(defun vterm-compile (&optional dir)
  "Compile the project in a vterm terminal at DIR.
If DIR is provided, switch to it before compilation."
  (interactive)
  (let* ((command compile-command))
    (setq compile-command (compilation-read-command command))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (with-current-buffer (vterm)
        (when dir
          (vterm-send-string (concat "cd " dir))
          (vterm-send-return))
      (setq vterm-compile-buffer (current-buffer))
      (rename-buffer (if dir
                         (format "*vterm %s compilation*" (file-name-nondirectory (directory-file-name dir)))
                       "*vterm compilation*"))
      (compilation-shell-minor-mode 1)
      (vterm-send-M-w)
      (vterm-send-string compile-command t)
      (vterm-send-return))))

(defun project-vterm-compile ()
  "Compile the project in a vterm terminal at the project's root."
  (interactive)
  (let* ((root (project-root (project-current t))))
    (vterm-compile root)))

(defun project-vterm ()
  "Open a `vterm` buffer in current project root dir"
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (buffer-name (format "*vterm - %s*" (project-name pr))))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (with-current-buffer (vterm (generate-new-buffer-name buffer-name))
        (vterm-send-string (concat "cd " root))
        (vterm-send-return)))))

(define-key project-prefix-map (kbd "v") #'project-vterm)
(define-key project-prefix-map (kbd "c") #'project-vterm-compile)

(setq custom-file (make-temp-file "custom.el"))
