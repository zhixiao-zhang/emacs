;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

(require 'use-package)

(setq use-package-always-ensure t)

(setq custom-file (make-temp-file "custom.el"))

(provide 'zz-core)
