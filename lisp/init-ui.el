(set-face-attribute 'default nil
                    :family "FiraCode Nerd Font"
                    :height 180)

(load-theme 'whiteboard)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items nil)
  (setq dashboard-banner-logo-title "Happy Hacking Systems: Get your hands DIRTY!")
  (setq dashboard-startup-banner "~/Pictures/dont-leave-me-alone.png")
  (setq dashboard-footer-messages '("Don't be satisfied with CONFERENCEWARE!"))
  (setq dashboard-center-content t))

(provide 'init-ui)
