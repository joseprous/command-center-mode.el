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

(defface command-center-heading
  '((((class color) (background light)) :foreground "DarkGoldenrod4" :weight bold)
    (((class color) (background  dark)) :foreground "LightGoldenrod2" :weight bold))
  "Face for section headings."
  :group 'command-center-faces)

(defface command-center-up
  '((((class color) (background light)) :foreground "green3" :weight bold)
    (((class color) (background  dark)) :foreground "green3" :weight bold))
  "Face for section headings."
  :group 'command-center-faces)

(defface command-center-down
  '((((class color) (background light)) :foreground "firebrick3" :weight bold)
    (((class color) (background  dark)) :foreground "firebrick3" :weight bold))
  "Face for section headings."
  :group 'command-center-faces)


(defun command-center-insert-command (command)
  "Insert the COMMAND."
  (if (get-process command)
      (progn
        (insert (concat command))
        (insert (propertize " running\n" 'face 'command-center-up))
        )
    (insert (concat command "\n"))
    )
  )

(defun command-center-service-isup (service)
  "Check if SERVICE is up."
  (if (= (funcall (plist-get service :status)) 0)
      t
    nil
    )
  )

(defun command-center-insert-service (service)
  "Insert the SERVICE."
  (insert (plist-get service :name))
  (if (command-center-service-isup service)
      (insert (propertize " up\n" 'face 'command-center-up))
    (insert (propertize " down\n" 'face 'command-center-down)))
  )

(defun command-center-refresh-buffer ()
  "Refresh command center mode."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (let ((commands-heading "Commands\n"))
      (insert (propertize commands-heading 'face 'command-center-heading))
      )
    (mapc 'command-center-insert-command (mapcar 'car command-center-commands))
    (let ((services-heading "Services\n"))
      (insert (propertize services-heading 'face 'command-center-heading))
      )
    (mapc 'command-center-insert-service command-center-services)
    )
  )

(defun command-center-get-command-index ()
  "Get the index of the command at point."
  (- (line-number-at-pos) 2)
  )

(defun command-center-get-service-index ()
  "Get the index of the service at point."
  (- (line-number-at-pos) (+ 3 (length command-center-commands)))
  )

(defun command-center-run-command ()
  "Execute command or run/stop service."
  (interactive)
  (let ((inhibit-read-only t))
    (let ((command-index (command-center-get-command-index)))
      (if (and (>= command-index 0) (< command-index (length command-center-commands)))
          (let ((name (car (nth command-index command-center-commands))))
            (if (get-process name)
                (progn
                  (message (concat "killing " name))
                  (delete-process name)
                  )
              (progn
                (message (concat "running " name))
                (funcall (cdr (assoc name command-center-commands)))
                )
              )
            )
        )
      )
    (let ((service-index (command-center-get-service-index)))
      (if (and (>= service-index 0) (< service-index (length command-center-services)))
          (let ((service (nth service-index command-center-services)))
            (if (command-center-service-isup service)
                (progn
                  (message (concat "stoping " (plist-get service :name)))
                  (funcall (plist-get service :stop))
                  )
              (progn
                (message (concat "running " (plist-get service :name)))
                (funcall (plist-get service :run))
                )
              )
            )
        )
      )
    )
  )

(defun command-center-send-enter ()
  "Send enter to process."
  (interactive)
  (let ((command-index (command-center-get-command-index)))
    (if (and (>= command-index 0) (< command-index (length command-center-commands)))
        (let ((name (car (nth command-index command-center-commands))))
          (when (get-process name)
              (progn
                (message (concat "sending enter " name))
                (process-send-string name "\n")
                )
            )
          )
      )
    )
  )

(if command-center-map
    nil
  (setq command-center-map (make-sparse-keymap))
  (define-key command-center-map [?g] 'command-center-refresh-buffer)
  (define-key command-center-map [?r] 'command-center-run-command)
  (define-key command-center-map [?e] 'command-center-send-enter)
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
