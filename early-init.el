;; -*- lexical-binding: t; -*-
(setq window-resize-pixelwise t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message user-login-name
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      use-dialog-box nil
      default-frame-alist '((width . 170)
                            (height . 60)
                            (horizontal-scroll-bar . nil)
			                      (vertical-scroll-bar . nil)
			                      (ns-transparent-titlebar t)))
(setopt menu-bar-mode nil
        scroll-bar-mode nil
        tool-bar-mode nil
        tooltip-mode nil)
