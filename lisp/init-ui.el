(set-face-attribute 'default nil
                    :family "FiraCode Nerd Font"
                    :height 160)

(add-to-list 'custom-theme-load-path "~/.emacs.d/everforest")
(load-theme 'everforest-hard-light t)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  ;; :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-items nil)
(setq dashboard-banner-logo-title "Happy Hacking Systems: Get your hands DIRTY!")
(setq dashboard-startup-banner "~/Pictures/xiaolan3.jpeg")
(setq dashboard-center-content t)

(provide 'init-ui)
