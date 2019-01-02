;;; command-center-mode --- Mode for running commands
;;; Commentary:

;;; Code:

(require 'derived)

(defun command-center ()
  "."
  (interactive)
  (switch-to-buffer "*command-center*")
  (command-center-mode))

(define-derived-mode command-center-mode special-mode "Command Center"
  "Mode for running commands"
  
  )

(provide 'command-center-mode)
;;; command-center-mode.el ends here
