(use-package treesit-auto
  :hook
  (after-init . global-treesit-auto-mode)
  :init
  (setq treesit-font-lock-level 4)
  :config
  (dolist (remap '((c++-mode . c-ts-mode)
                  (html-mode . mhtml-mode)))
          (add-to-list 'major-mode-remap-alist remap))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

(use-package eldoc-box
  :hook ((eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-mode . eldoc-box-hover-mode)))

(use-package eglot
  :hook ((c-ts-mode c++-ts-mode) . eglot-ensure)
  :config (with-eval-after-load 'eglot            (dolist (mode-server '((c-ts-mode . ("clangd" "--header-insertion=never"))
                                   (c++-ts-mode . ("clangd" "--header-insertion=never"))))
              (add-to-list 'eglot-server-programs mode-server))))

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

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main")
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
	      ("C-c" . copilot-accept-completion))
  :custom (copilot-indent-offset-warning-disable t)
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode . 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(yaml-mode . 2))
  (add-to-list 'copilot-indentation-alist '(python-ts-mode . 4))
  (add-to-list 'copilot-indentation-alist '(markdown-mode . 4)))

(provide 'init-programming)
