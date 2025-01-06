(use-package treesit-auto
  :demand
  :hook
  (after-init . global-treesit-auto-mode)
  :init
  (progn
    (setq treesit-font-lock-level 4)
    (add-to-list 'major-mode-remap-alist '(c++-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(html-mode . mhtml-mode))
    (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
    ))

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
  :config
  (define-key copilot-completion-map (kbd "C-c") 'copilot-accept-completion))
(provide 'init-programming)
