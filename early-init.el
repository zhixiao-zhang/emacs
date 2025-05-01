;; -*- lexical-binding: t; -*-
(setq read-process-output-max (* 4 1024 1024)
      display-time-load-average nil
      process-adaptive-read-buffering nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      default-frame-alist '((width . 150)
			                      (height . 50)
			                      (vertical-scroll-bar . nil)
			                      (ns-transparent-titlebar t)))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
