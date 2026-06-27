;; -*- lexical-binding: t; -*-

(defun my/apply-font-settings (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (let* ((font-size 18)
             (latin-font (cond ((eq system-type 'darwin) "SF Mono")
                               ((eq system-type 'gnu/linux) "SFMono Nerd Font Mono")
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
      mouse-wheel-progressive-speed nil)

(pixel-scroll-mode 1)

(use-package dashboard
  :if (display-graphic-p)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline
                                    dashboard-insert-init-info))
  (setq dashboard-banner-logo-title "你枉读诗书习经典，岂不知非礼勿能言。")
  (setq dashboard-startup-banner "~/.emacs.d/dlma.png"))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

(provide 'zz-ui)
