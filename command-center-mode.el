;;; command-center-mode --- Mode for running commands
;;; Commentary:

;;; Code:

(require 'derived)
(defvar command-center-map nil
  "Keymap for command center major mode.")

(defvar command-center-commands nil
  "Commands for command center major mode."
  )

(defvar command-center-services nil
  "Services for command center major mode."
 )

(defun command-center-insert-command (command)
  "Insert the COMMAND."
  (insert command)
  )

(defun command-center-insert-service (service)
  "Insert the SERVICE."
  (insert (plist-get service :name))
  (if (= (funcall (plist-get service :status)) 0)
      (insert " up\n")
    (insert " down\n"))
  )

(defun command-center-refresh-buffer ()
  "Refresh command center mode."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Commands\n")
    (mapc 'command-center-insert-command (mapcar 'car command-center-commands))
    (insert "Services\n")
    (mapc 'command-center-insert-service command-center-services)
    )
  )

(defun command-center-get-command-index ()
  "Get the index of the command at point."
  (- (line-number-at-pos) 2)
  )

(defun command-center-run-command ()
  "Refresh command center mode."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((name (car (nth (command-center-get-command-index) command-center-commands))))
      (message (concat "runing " name))
      (funcall (cdr (assoc name command-center-commands)))
      )
    )
  )

(if command-center-map
    nil
  (setq command-center-map (make-sparse-keymap))
  (define-key command-center-map [?g] 'command-center-refresh-buffer)
  (define-key command-center-map [?r] 'command-center-run-command)
  )

(if command-center-commands
    nil
  (setq command-center-commands
        '(("start-postgres\n" . (lambda () (start-process "postgres" "*psql*" "c:/Users/prousj/programas/pgsql/bin/pg_ctl.exe" "start" "-D" "c:/Users/prousj/Documents/psql/data/" "-l" "c:/Users/prousj/Documents/psql/archivo_de_registro")))
          ("stop-postgres\n" . (lambda () (start-process "postgres" "*psql*" "c:/Users/prousj/programas/pgsql/bin/pg_ctl.exe" "stop" "-D" "c:/Users/prousj/Documents/psql/data/")))
          )
        )
)

(if command-center-services
    nil
  (setq command-center-services
        '((:name "postgres"
                 :start (lambda () (start-process "postgres" "*psql*" "c:/Users/prousj/programas/pgsql/bin/pg_ctl.exe" "start" "-D" "c:/Users/prousj/Documents/psql/data/" "-l" "c:/Users/prousj/Documents/psql/archivo_de_registro"))
                 :stop (lambda () (start-process "postgres" "*psql*" "c:/Users/prousj/programas/pgsql/bin/pg_ctl.exe" "stop" "-D" "c:/Users/prousj/Documents/psql/data/"))
                 :status (lambda () (call-process "c:/Users/prousj/programas/pgsql/bin/pg_isready.exe" nil nil nil "-q"))
                 )
          )
        )
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
