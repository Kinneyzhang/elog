(defconst elog-level-list '(trace debug info warning error)
  "All levels of emacs log.")

(defconst elog-default-buffer "*elog*"
  "Buffer to show emacs log.")

(defvar elog-default-level nil
  "Current level of elog, level higher than
this level will be shown in `elog-default-buffer'.")

;; (defvar elog-max-log-num 10000)

;;; utils

(defun format-time-millis ()
  "返回当前时间的字符串表示，精确到毫秒。"
  (let* ((now (float-time))
         (seconds (floor now))
         (millis (floor (* (- now seconds) 1000))))
    (concat (format-time-string "%H:%M:%S" seconds)
            "." (format "%03d" millis))))

;;; functions

(cl-defun elog-logger (&key (level 'debug)
                            (buffer elog-default-buffer))
  (list :level level :buffer buffer))

(defun elog-set-level (logger level)
  (setq logger (plist-put logger :level level)))

(defun elog-set-buffer (logger buffer)
  (setq logger (plist-put logger :buffer buffer)))

(defun elog (logger level format-string args)
  (with-current-buffer (get-buffer-create (plist-get logger :buffer))
    (local-set-key "q" 'elog-quit)
    (goto-char (point-max))
    (when-let ((allowed-level (plist-get logger :level)))
      (let ((allowed-levels (member allowed-level elog-level-list))
            (inhibit-read-only 1))
        (when (member level allowed-levels)
          (unless (bolp) (newline))
          (insert (format "%s| [%s] %s\n"
                          (format-time-millis)
                          (upcase (symbol-name level))
                          (apply 'format format-string args))))))
    (read-only-mode 1)))

(defun elog-trace (logger format-string &rest args)
  (declare (indent defun))
  (elog logger 'trace format-string args)
  t)

(defun elog-debug (logger format-string &rest args)
  (declare (indent defun))
  (elog logger 'debug format-string args)
  t)

(defun elog-info (logger format-string &rest args)
  (declare (indent defun))
  (elog logger 'info format-string args)
  t)

(defun elog-warning (logger format-string &rest args)
  (declare (indent defun))
  (elog logger 'warning format-string args)
  t)

(defun elog-error (logger format-string &rest args)
  (declare (indent defun))
  (elog logger 'error format-string args))

(defun elog-quit ()
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (with-current-buffer buffer
      (local-unset-key "q"))))

(defun elog-log-view (logger)
  (switch-to-buffer
   (get-buffer-create (plist-get logger :buffer)))
  (local-set-key "q" 'elog-quit))

(defun elog-log-clear (logger)
  (with-current-buffer
      (get-buffer-create (plist-get logger :buffer))
    (let ((inhibit-read-only t))
      (erase-buffer))))

(provide 'elog)
