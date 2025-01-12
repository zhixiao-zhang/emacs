(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-packages)
(require 'init-ui)
(require 'init-editor)
(require 'llvm)
(require 'init-programming)


(setq custom-file (make-temp-file "custom.el"))
