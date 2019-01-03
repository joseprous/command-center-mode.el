;;; command-center-mode --- Mode for running commands
;;; Commentary:

;;; Code:

(require 'derived)
(defvar command-center-map nil
  "Keymap for command center major mode.")

(defun command-center-refresh-buffer ()
  "Refresh command center mode."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Hello world!\n")
    )
  )

(defun command-center-run-command ()
  "Refresh command center mode."
  (interactive)
  (let ((inhibit-read-only t))
    (insert "run command\n")
    )
  )

(if command-center-map
    nil
  (setq command-center-map (make-sparse-keymap))
  (define-key command-center-map [?g] 'command-center-refresh-buffer)
  (define-key command-center-map [?r] 'command-center-run-command)
  )

(defun command-center ()
  "Start command center mode."
  (interactive)
  (switch-to-buffer "*command-center*")
  (command-center-refresh-buffer)
  (command-center-mode))

(define-derived-mode command-center-mode special-mode "Command Center"
  "Mode for running commands"
  (use-local-map command-center-map)
  )

(provide 'command-center-mode)
;;; command-center-mode.el ends here
