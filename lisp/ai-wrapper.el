;; -*- lexical-binding: t; -*-
(defun my/get-project-or-directory ()
  "Return project root if available, else current buffer's directory."
  (if-let ((proj (project-current)))
      (project-root proj)
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      default-directory)))

(defun aiagent (alias-name)
  "Run a fish alias with support for multiple rounds of conversation.
Provides a dedicated editing buffer for each round of input."
  (interactive "sFish alias name: ")
  (let* ((process-buffer (get-buffer-create (concat "*" alias-name "*")))
         (cur-path (my/get-project-or-directory))
         (script-file (make-temp-file "emacs-fish-" nil ".fish"))
         (proc-alive (comint-check-proc process-buffer))
         ;; Store the original window's config
         (orig-window-config (current-window-configuration))
         (new-input-buffer-name 
          (lambda () 
            (generate-new-buffer-name (concat "*" alias-name "-input*")))))
    
    ;; Create a temproray script file
    (with-temp-file script-file
      (insert (concat "cd " cur-path) " && ")
      (insert alias-name "\n"))
    
    ;; Set the process buffer
    (unless proc-alive
      (with-current-buffer process-buffer
        (unless (comint-check-proc process-buffer)
          ;; use pty
          (let ((process-connection-type t))
            (make-comint-in-buffer alias-name process-buffer "fish" nil script-file))
          
          ;; enable comint-mode
          (unless (eq major-mode 'comint-mode)
            (comint-mode))
          
          ;; type C-c C-r to start a new round input
          (local-set-key (kbd "C-c C-r") 
                         (lambda ()
                           (interactive)
                           (chat-new-round process-buffer orig-window-config alias-name)))
          
          ;; add some help message
          (let ((inhibit-read-only t))
            (goto-char (point-max))))))
    
    ;; cleanup the temporary file
    (run-with-timer 2 nil (lambda () (delete-file script-file)))
    
    ;; start a new round
    (chat-new-round process-buffer orig-window-config alias-name)
    
    ;; go back to the process buffer
    process-buffer))

(defun chat-new-round (process-buffer orig-window-config alias-name)
  "Create a new input buffer for another round of conversation."
  (let ((input-buffer (generate-new-buffer (concat "*" alias-name "-input*"))))
    
    ;; Set the input buffer
    (with-current-buffer input-buffer
      (text-mode)
      (insert ";; Type C-c C-c to send your input to the agent\n")
      (insert ";; Type C-c C-k to cancel this turn\n")
      (insert ";; Send empty content by directly typing C-c C-c!\n\n")
      
      ;; send
      (local-set-key (kbd "C-c C-c")
                     (lambda ()
                       (interactive)
                       (let* ((content-start (save-excursion
                                               (goto-char (point-min))
                                               (forward-line 4)
                                               (point)))
                              (content (buffer-substring-no-properties 
                                        content-start
                                        (point-max)))
                              (content-trimmed (string-trim content))
                              (proc (get-buffer-process process-buffer)))
                         
                         ;; switch to the process-buffer
                         (with-current-buffer process-buffer
                           (let ((inhibit-read-only t))
                             (goto-char (point-max))))
                         
                         (when (> (length content-trimmed) 0)
                           (process-send-string proc content))
                         
                         (run-with-timer 0.1 nil 
                                         (lambda ()
                                           (process-send-string proc "\C-l")
                                           (with-current-buffer process-buffer
                                             (let ((inhibit-read-only t))
                                               (goto-char (point-max))))))
                         
                         (kill-buffer input-buffer)
                         (let ((proc-window (get-buffer-window process-buffer)))
                           (if proc-window
                               (select-window proc-window)
                             (pop-to-buffer process-buffer))))))
      
      (local-set-key (kbd "C-c C-k")
                     (lambda ()
                       (interactive)
                       (kill-buffer input-buffer)
                       (let ((proc-window (get-buffer-window process-buffer)))
                         (if proc-window
                             (select-window proc-window)
                           (pop-to-buffer process-buffer))))))
    
    (switch-to-buffer input-buffer)))

(provide 'ai-wrapper)
