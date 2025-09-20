;; -*- lexical-binding: t; -*-
(define-advice use-package
    (:around (orig package &rest body) use-with-binary)
  (let ((executable (plist-get body :with)))
    (when executable
      (setq body (seq-difference body `(:with ,executable))))
    (if (or (not executable) (executable-find executable))
        (apply orig package body))))

(use-package org
  :load-path "~/.emacs.d/elpa/org-mode/lisp/"
  :config
  (add-hook 'org-mode-hook 'org-latex-preview-mode)
  (setq org-latex-preview-live t))

(defun my/apply-font-settings (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (let* ((font-size 19)
             (latin-font (cond ((eq system-type 'darwin) "SF Mono")
                               ((eq system-type 'gnu/linux) "SFMono")
                               (t nil)))
             (cjk-font (when (member "LXGW WenKai" (font-family-list))
                         "LXGW WenKai")))
        (when latin-font
          (set-frame-font (format "%s-%d" latin-font font-size) t t))
        (when cjk-font
          (dolist (charset '(kana han cjk-misc bopomofo))
            (set-fontset-font t charset (font-spec :family cjk-font :size font-size))))))))

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

(defun open-reading-list ()
  "Open my reading list file"
  (interactive)
  (find-file "~/Documents/academic/reading-list.org"))

(global-set-key (kbd "C-c i") 'open-init-file)
(global-set-key (kbd "C-c d") 'my-delete-space-to-next-char)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "M-《") 'beginning-of-buffer)
(global-set-key (kbd "M-》") 'end-of-buffer)
(global-set-key (kbd "C-c r") 'open-reading-list)
(global-set-key (kbd "C-x C-u") 'undo-redo)

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

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package magit
  :defer t)

(use-package valign
  :defer t)

(defun kill-buffers-matching-glob (pattern)
  "Kill all buffers whose file names match the given glob PATTERN.
Example: *.ll"
  (interactive "sGlob pattern (e.g., *.ll): ")
  (let* ((regexp (wildcard-to-regexp pattern))
         (buffers (cl-remove-if-not
                   (lambda (buf)
                     (let ((fname (buffer-file-name buf)))
                       (and fname (string-match-p regexp fname))))
                   (buffer-list))))
    (if buffers
        (progn
          (dolist (buf buffers)
            (kill-buffer buf))
          (message "Killed %d buffers matching: %s" (length buffers) pattern))
      (message "No buffers matched: %s" pattern))))

(use-package pcmpl-args
  :defer t)

(use-package olivetti
  :defer t)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

(setq custom-file (make-temp-file "custom.el"))
