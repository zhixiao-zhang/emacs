(use-package treesit-auto
  :hook
  (after-init . global-treesit-auto-mode)
  :init
  (setq treesit-font-lock-level 4)
  :config
  (dolist (remap '((html-mode . mhtml-mode)))
          (add-to-list 'major-mode-remap-alist remap))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

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

(add-to-list 'c-default-style '(c-ts-mode . "llvm.org"))
(add-to-list 'c-default-style '(c++-ts-mode . "llvm.org"))

(provide 'init-programming)
