;;; pi.el --- Minimal RPC bridge to the pi coding agent -*- lexical-binding: t; -*-

;; Bare-bones: launches `pi --mode rpc`, logs every JSON line it emits
;; into a buffer, and lets you send prompts to it. Only one process
;; at a time.

(defvar pi-process nil
  "The single running pi RPC process, or nil.")

(defvar pi-buffer-name "*pi-rpc*"
  "Name of the buffer that logs pi RPC output.")

(defvar pi-command '("pi" "--mode" "rpc" "-e" "./emacs-bridge.ts")
  "Command used to launch pi in RPC mode.")

(defun pi--buffer ()
  (get-buffer-create pi-buffer-name))

(defun pi--log (line)
  (with-current-buffer (pi--buffer)
    (goto-char (point-max))
    (insert line "\n")))

(defun pi--filter (_proc chunk)
  (dolist (line (split-string chunk "\n" t))
    (pi--log line)))

(defun pi--sentinel (_proc event)
  (pi--log (format ";; process event: %s" (string-trim event)))
  (setq pi-process nil))

(defun pi-start ()
  "Start the pi RPC process. Errors if one is already running."
  (interactive)
  (when (process-live-p pi-process)
    (user-error "pi is already running (use M-x pi-kill first)"))
  (setq pi-process
        (make-process
         :name "pi-rpc"
         :buffer nil
         :command pi-command
         :connection-type 'pipe
         :coding 'utf-8
         :filter #'pi--filter
         :sentinel #'pi--sentinel))
  (pi--log ";; pi started")
  (display-buffer (pi--buffer))
  pi-process)

(defun pi-send-prompt (message)
  "Send MESSAGE as a prompt to the running pi process, starting it if needed."
  (interactive "sPrompt: ")
  (unless (process-live-p pi-process)
    (pi-start))
  (let ((json (json-encode `((type . "prompt") (message . ,message)))))
    (pi--log (format ">> %s" json))
    (process-send-string pi-process (concat json "\n"))))

(defun pi-kill ()
  "Kill the running pi process, if any."
  (interactive)
  (if (process-live-p pi-process)
      (progn
        (delete-process pi-process)
        (setq pi-process nil)
        (pi--log ";; pi killed"))
    (message "pi is not running")))

(provide 'pi)
;;; pi.el ends here
