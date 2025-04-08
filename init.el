(when (display-graphic-p)
  (cond
   ((eq system-type 'darwin)
    (set-frame-font "SF Mono-18" t t))
   ((eq system-type 'gnu/linux)
    (set-frame-font "SFMono Nerd Font Mono-18" t t))))

(when (member "LXGW WenKai" (font-family-list))
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset (font-spec :family "LXGW WenKai" :size 18))))

(load "~/.emacs.d/light-pink-theme.el")
(load-theme 'light-pink t)

(setq mode-line-right-align-edge 'right-margin)

(defun mode-line-compose (indicator buffer preamble postamble)
  (list `(:propertize ,indicator face (:foreground "#F5F5F5" :background "#E17092"))
        `(:propertize ,buffer face (:foreground "#9466AA"))
        'mode-line-format-right-align
        `(:propertize ,preamble face (:foreground "#C46B81"))
        '(:propertize "<<" face (:foreground "#AAAAAA"))
        `(:propertize ,postamble face ((:foreground "#E17092") bold))))

(defun mode-line-default ()
  (let ((indicator '(" " (:eval (cond ((and buffer-file-name (buffer-modified-p)) "RW")
                                      (buffer-read-only "RO")
                                      (t "WR"))) " "))
        (buffer '(" %b "))
        (preamble '((-3 "%p") " %l:%c "))
        (postamble '(" %[" mode-name "%] ")))
    (mode-line-compose indicator buffer preamble postamble)))

(setq-default mode-line-format (mode-line-default))

(defun adjust-frame-opacity (frame delta)
  (let* ((alpha (or (frame-parameter frame 'alpha) 100))
         (alpha (if (listp alpha) (car alpha) alpha))
         (alpha (+ alpha delta)))
    (when (and (<= frame-alpha-lower-limit alpha)
               (>= 100 alpha))
      (set-frame-parameter frame 'alpha alpha))))

(defun frame-perspective ()
  (interactive)
  (dolist (frame (frame-list))
    (adjust-frame-opacity frame -40))
  (read-event)
  (when last-input-event
    (dolist (frame (frame-list))
      (adjust-frame-opacity frame 40))))

(keymap-global-set "M-RET" 'frame-perspective)

(setq auto-save-default nil
      make-backup-files nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil
      inhibit-startup-screen t
      inhibit-compacting-font-caches t)

(setq-default indent-tabs-mode nil
              tab-width 2)

(setq word-wrap-by-category t)
(select-frame-set-input-focus (selected-frame))

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'text-mode-hook 'hl-line-mode)

(defun text-mode-refine ()
  (setq-local corfu-auto nil)
  (visual-line-mode))

(add-hook 'text-mode-hook
          'text-mode-refine)

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
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c f") 'windmove-right)
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
                      :background "#FEF3C7")
  (setq eldoc-box-max-pixel-width 500)
  (setq eldoc-box-max-pixel-height 300))

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

(use-package web-mode
  :defer t
  :mode ("\\.html?\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

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
              markdown-fontify-code-blocks-natively t))

(with-eval-after-load 'markdown-mode
  (set-face-underline 'markdown-line-break-face nil))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  dashboard-insert-init-info))
  (setq dashboard-banner-logo-title "你枉读诗书习经典，岂不知非礼勿能言。")
  (setq dashboard-startup-banner "~/Pictures/dlma.png"))

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
  (let* ((command compile-command)
         (buffer-name (if dir
                          (format "*vterm %s compilation*" (file-name-nondirectory (directory-file-name dir)))
                        "*vterm compilation*")))
         (setq compile-command (compilation-read-command command))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (with-current-buffer (vterm)
        (when dir
          (vterm-send-string (concat "cd " dir))
          (vterm-send-return))
      (setq vterm-compile-buffer (current-buffer))
      (rename-buffer buffer-name)
      (compilation-shell-minor-mode 1)
      (vterm-send-M-w)
      (vterm-send-string compile-command t)
      (vterm-send-return)))))

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

(with-eval-after-load 'project
  (define-key project-prefix-map "v" #'project-vterm)
  (add-to-list 'project-switch-commands '(project-vterm "vterm") t)
  (setq project-switch-commands
        (cl-remove-if
         (lambda (command) (memq (car-safe command) '(project-eshell project-vc-dir project-any-command)))
         project-switch-commands)))

(setq custom-file (make-temp-file "custom.el"))
