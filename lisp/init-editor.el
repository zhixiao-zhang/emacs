(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)

(global-auto-revert-mode t)
(delete-selection-mode t)
(electric-pair-mode t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-shell-name "/opt/homebrew/bin/fish")
  (setq exec-path-from-shell-variables'("PATH" "MANPATH" "LDFLAGS" "SDKROOT" "CPPFLAGS" "HOMEBREW_PREFIX" "HOMEBREW_CELLAR" "HOMEBREW_REPOSITORY" "INFOPATH"))
  :config
  (exec-path-from-shell-initialize))

(use-package recentf
  :hook (after-init . recentf-mode)
  :bind ("C-x b" . consult-buffer)
  :init
  (setq recentf-max-menu-items 10))

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

(use-package embark
  :bind
  ("C-;" . embark-act)
  (:map minibuffer-local-map
    ("C-c C-e" . embark-export-write))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package consult
  :bind
  ("C-s" . consult-line))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

(defun embark-export-write ()
  "Custom writable export for Embark."
  (interactive)
  (require 'embark)
  (require 'wgrep)
  (pcase-let ((`(,type . ,candidates)
              (run-hook-with-args-until-success 'embark-candidate-collectors)))
    (pcase type
      ('consult-grep (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
        (embark-export)))
      ('file (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
        (embark-export)))
      ('consult-location (let ((embark-after-export-hook #'occur-edit-mode))
        (embark-export)))
      (x (user-error "embark category %s doesn't support writable export" x)))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(pixel-scroll-mode 1)
(global-display-line-numbers-mode t)

(use-package magit
  :with "git"
  :defer t)

(provide 'init-editor)
