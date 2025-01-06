(with-eval-after-load 'package
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/")))

(eval-when-compile
  (require 'use-package))

(defun use-package-with-executable (orig package &rest body)
  (let ((exec (plist-get body :with)))
    (when exec
      (setq body (seq-difference body (list :with exec))))
    (if (or (not exec) (executable-find exec))
        (apply orig package body))))

(advice-add 'use-package :around #'use-package-with-executable)

(setq use-package-always-ensure t)

(provide 'init-packages)
