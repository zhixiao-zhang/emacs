(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)

(global-auto-revert-mode t)
(delete-selection-mode t)
(setq auto-save-default nil)
(setq make-backup-files nil)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)

(use-package smartparens
  :ensure smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :init
  (exec-path-from-shell-initialize))

(use-package recentf
  :hook (after-init . recentf-mode)
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

(use-package magit
  :with "git"
  :defer t)

(provide 'init-editor)
