;;; -*- lexical-binding: t; -*-

;;; elog.el --- A powerful logging system for Emacs Lisp

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Kinney Zhang
;; Version: 2.0.0
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5"))
;; Keywords: tools, logging
;; URL: https://github.com/Kinneyzhang/elog

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; elog is a powerful logging system for Emacs Lisp, inspired by mainstream
;; logging systems like Log4j, Winston, and loguru. It provides:
;;
;; - Multiple log levels: TRACE, DEBUG, INFO, WARNING, ERROR, FATAL
;; - Named loggers for identifying log sources
;; - Multiple output handlers: buffer, file, message area
;; - Customizable log format patterns
;; - Context/extra data support (similar to MDC in Log4j)
;; - Log buffer size limit with automatic rotation
;; - Colored output for visual distinction of log levels
;; - Caller information for debugging
;; - Conditional logging for performance optimization
;;
;; Basic usage:
;;
;;   (require 'elog)
;;   (setq my-logger (elog-logger :name "myapp" :level 'info))
;;   (elog-info my-logger "Application started")
;;   (elog-error my-logger "Error occurred: %s" error-message)
;;
;; With context:
;;
;;   (elog-with-context my-logger '(:user "john" :action "login")
;;     (elog-info my-logger "User action recorded"))

;;; Code:

(require 'cl-lib)

;;;; Constants and Variables

(defconst elog-level-list '(trace debug info warning error fatal)
  "All levels of emacs log, from lowest to highest severity.
TRACE - Fine-grained debugging information
DEBUG - Debugging information
INFO  - General informational messages
WARNING - Warning messages for potentially harmful situations
ERROR - Error messages for error events
FATAL - Severe error messages for critical failures")

(defconst elog-level-priority
  '((trace . 0) (debug . 1) (info . 2) (warning . 3) (error . 4) (fatal . 5))
  "Priority values for log levels, higher means more severe.")

(defconst elog-default-buffer "*elog*"
  "Buffer to show emacs log.")

(defvar elog-default-level nil
  "Current level of elog, level higher than
this level will be shown in `elog-default-buffer'.")

(defvar elog-default-format "%t| [%l] <%n> %m"
  "Default log format pattern.
Available placeholders:
  %t - Timestamp (HH:MM:SS.mmm)
  %T - Full timestamp (YYYY-MM-DD HH:MM:SS.mmm)
  %l - Log level (uppercase)
  %n - Logger name
  %m - Log message
  %c - Context data
  %f - Source file name (if available)
  %L - Source line number (if available)
  %F - Source function name (if available)")

(defvar elog-max-buffer-lines 10000
  "Maximum number of lines to keep in log buffer.
When exceeded, oldest lines are removed. Set to nil to disable.")

(defvar elog-colorize t
  "Whether to colorize log output in buffers.")

(defvar elog-include-caller nil
  "Whether to include caller information in log messages.")

;;;; Faces for colored output

(defgroup elog nil
  "A powerful logging system for Emacs Lisp."
  :group 'tools
  :prefix "elog-")

(defface elog-trace-face
  '((t :foreground "gray60"))
  "Face for TRACE level logs."
  :group 'elog)

(defface elog-debug-face
  '((t :foreground "cyan"))
  "Face for DEBUG level logs."
  :group 'elog)

(defface elog-info-face
  '((t :foreground "green"))
  "Face for INFO level logs."
  :group 'elog)

(defface elog-warning-face
  '((t :foreground "orange"))
  "Face for WARNING level logs."
  :group 'elog)

(defface elog-error-face
  '((t :foreground "red"))
  "Face for ERROR level logs."
  :group 'elog)

(defface elog-fatal-face
  '((t :foreground "red" :weight bold :underline t))
  "Face for FATAL level logs."
  :group 'elog)

(defvar elog-level-faces
  '((trace . elog-trace-face)
    (debug . elog-debug-face)
    (info . elog-info-face)
    (warning . elog-warning-face)
    (error . elog-error-face)
    (fatal . elog-fatal-face))
  "Mapping of log levels to their corresponding faces.")

;;;; Context (MDC-like functionality)

(defvar elog-global-context nil
  "Global context data available to all loggers.
This is an alist of key-value pairs.")

(defvar-local elog-local-context nil
  "Buffer-local context data.
This is an alist of key-value pairs.")

(defun elog-set-global-context (key value)
  "Set a global context KEY to VALUE."
  (setq elog-global-context
        (cons (cons key value)
              (assq-delete-all key elog-global-context))))

(defun elog-remove-global-context (key)
  "Remove KEY from global context."
  (setq elog-global-context (assq-delete-all key elog-global-context)))

(defun elog-clear-global-context ()
  "Clear all global context data."
  (setq elog-global-context nil))

(defun elog-get-context (logger)
  "Get merged context for LOGGER (logger context + global + local)."
  (let ((logger-ctx (plist-get logger :context)))
    (append logger-ctx elog-local-context elog-global-context)))

;;;; Utils

(defun elog--format-time-millis ()
  "Return current time as string with millisecond precision (HH:MM:SS.mmm)."
  (let* ((now (float-time))
         (seconds (floor now))
         (millis (floor (* (- now seconds) 1000))))
    (concat (format-time-string "%H:%M:%S" seconds)
            "." (format "%03d" millis))))

(defun elog--format-time-full ()
  "Return full timestamp with date and milliseconds (YYYY-MM-DD HH:MM:SS.mmm)."
  (let* ((now (float-time))
         (seconds (floor now))
         (millis (floor (* (- now seconds) 1000))))
    (concat (format-time-string "%Y-%m-%d %H:%M:%S" seconds)
            "." (format "%03d" millis))))

;; Keep old function name for backward compatibility
(defalias 'format-time-millis 'elog--format-time-millis)

(defun elog--get-caller-info ()
  "Get caller information (file, line, function).
Returns a plist with :file, :line, and :function keys."
  (let* ((frames (backtrace-frames))
         (caller-frame nil)
         (elog-funcs '(elog elog-trace elog-debug elog-info
                           elog-warning elog-error elog-fatal
                           elog--get-caller-info elog--format-message
                           elog--log-to-buffer elog--log-to-file
                           elog--log-to-message)))
    ;; Find the first frame not belonging to elog functions
    (dolist (frame frames)
      (when (and (not caller-frame)
                 (car frame)
                 (symbolp (car frame))
                 (not (memq (car frame) elog-funcs)))
        (setq caller-frame frame)))
    (if caller-frame
        (list :function (symbol-name (car caller-frame))
              :file nil
              :line nil)
      (list :function "unknown" :file nil :line nil))))

(defun elog--context-to-string (context)
  "Convert CONTEXT alist to a human-readable string."
  (if context
      (mapconcat (lambda (pair)
                   (format "%s=%s" (car pair) (cdr pair)))
                 context " ")
    ""))

(defun elog--format-message (logger level message)
  "Format log MESSAGE according to LOGGER's format pattern and LEVEL."
  (let* ((format-pattern (or (plist-get logger :format) elog-default-format))
         (name (or (plist-get logger :name) "elog"))
         (context (elog-get-context logger))
         (context-str (elog--context-to-string context))
         (caller-info (when elog-include-caller (elog--get-caller-info)))
         (result format-pattern))
    ;; Replace format placeholders
    (setq result (replace-regexp-in-string "%t" (elog--format-time-millis) result t t))
    (setq result (replace-regexp-in-string "%T" (elog--format-time-full) result t t))
    (setq result (replace-regexp-in-string "%l" (upcase (symbol-name level)) result t t))
    (setq result (replace-regexp-in-string "%n" name result t t))
    (setq result (replace-regexp-in-string "%m" message result t t))
    (setq result (replace-regexp-in-string "%c" context-str result t t))
    (when caller-info
      (setq result (replace-regexp-in-string
                    "%F" (or (plist-get caller-info :function) "") result t t))
      (setq result (replace-regexp-in-string
                    "%f" (or (plist-get caller-info :file) "") result t t))
      (setq result (replace-regexp-in-string
                    "%L" (if (plist-get caller-info :line)
                             (number-to-string (plist-get caller-info :line))
                           "")
                    result t t)))
    result))

(defun elog--level-enabled-p (logger level)
  "Check if LEVEL is enabled for LOGGER."
  (let* ((allowed-level (plist-get logger :level))
         (allowed-priority (cdr (assq allowed-level elog-level-priority)))
         (level-priority (cdr (assq level elog-level-priority))))
    (and allowed-priority level-priority
         (>= level-priority allowed-priority))))

(defun elog--rotate-buffer (buffer)
  "Rotate log BUFFER if it exceeds `elog-max-buffer-lines'."
  (when (and elog-max-buffer-lines
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((line-count (count-lines (point-min) (point-max))))
        (when (> line-count elog-max-buffer-lines)
          (let ((inhibit-read-only t)
                (lines-to-remove (- line-count elog-max-buffer-lines)))
            (goto-char (point-min))
            (forward-line lines-to-remove)
            (delete-region (point-min) (point))))))))

(defun elog--apply-face (text level)
  "Apply appropriate face to TEXT based on LEVEL."
  (if elog-colorize
      (let ((face (cdr (assq level elog-level-faces))))
        (if face
            (propertize text 'face face)
          text))
    text))

(defun elog--log-to-buffer (buffer formatted-message level)
  "Write FORMATTED-MESSAGE to BUFFER with LEVEL-based formatting."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (local-set-key "q" 'elog-quit)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (newline))
        (insert (elog--apply-face formatted-message level))
        (newline))
      (elog--rotate-buffer buf)
      (read-only-mode 1))))

(defun elog--log-to-file (file formatted-message)
  "Append FORMATTED-MESSAGE to FILE."
  (let ((dir (file-name-directory file)))
    (when (and dir (not (file-exists-p dir)))
      (make-directory dir t)))
  (write-region (concat formatted-message "\n") nil file 'append 'quiet))

(defun elog--log-to-message (formatted-message)
  "Display FORMATTED-MESSAGE in the echo area using `message'."
  (message "%s" formatted-message))

;;;; Logger Creation and Configuration

(cl-defun elog-logger (&key (name "elog")
                            (level 'debug)
                            (buffer elog-default-buffer)
                            (file nil)
                            (format nil)
                            (handlers '(buffer))
                            (context nil))
  "Create a new logger with the specified configuration.

Keyword arguments:
  :name     - Logger name/tag for identification (default: \"elog\")
  :level    - Minimum log level to output (default: 'debug)
              One of: trace, debug, info, warning, error, fatal
  :buffer   - Buffer name for buffer handler (default: \"*elog*\")
  :file     - File path for file handler (default: nil)
  :format   - Custom format pattern (default: `elog-default-format')
  :handlers - List of output handlers (default: '(buffer))
              Available: buffer, file, message
  :context  - Initial context alist (default: nil)

Returns a logger plist that can be used with elog-* functions."
  (list :name name
        :level level
        :buffer buffer
        :file file
        :format format
        :handlers handlers
        :context context))

(defun elog-set-level (logger level)
  "Set the log LEVEL for LOGGER. Returns modified logger."
  (plist-put logger :level level))

(defun elog-set-buffer (logger buffer)
  "Set the output BUFFER for LOGGER. Returns modified logger."
  (plist-put logger :buffer buffer))

(defun elog-set-file (logger file)
  "Set the output FILE for LOGGER. Returns modified logger."
  (plist-put logger :file file))

(defun elog-set-format (logger format)
  "Set the log FORMAT pattern for LOGGER. Returns modified logger."
  (plist-put logger :format format))

(defun elog-set-handlers (logger handlers)
  "Set the output HANDLERS for LOGGER. Returns modified logger.
HANDLERS should be a list containing any of: buffer, file, message."
  (plist-put logger :handlers handlers))

(defun elog-add-handler (logger handler)
  "Add a HANDLER to LOGGER's handlers list. Returns modified logger."
  (let ((handlers (plist-get logger :handlers)))
    (unless (memq handler handlers)
      (plist-put logger :handlers (cons handler handlers))))
  logger)

(defun elog-set-context (logger context)
  "Set the CONTEXT for LOGGER. Returns modified logger.
CONTEXT should be an alist of key-value pairs."
  (plist-put logger :context context))

(defun elog-add-context (logger key value)
  "Add a context KEY-VALUE pair to LOGGER. Returns modified logger."
  (let ((ctx (plist-get logger :context)))
    (plist-put logger :context
               (cons (cons key value)
                     (assq-delete-all key ctx)))))

;;;; Core Logging Function

(defun elog (logger level format-string args)
  "Log a message at LEVEL using LOGGER.
FORMAT-STRING and ARGS are passed to `format' to create the message."
  (when (elog--level-enabled-p logger level)
    (let* ((message (apply #'format format-string args))
           (formatted (elog--format-message logger level message))
           (handlers (or (plist-get logger :handlers) '(buffer))))
      ;; Output to all configured handlers
      (dolist (handler handlers)
        (cond
         ((eq handler 'buffer)
          (elog--log-to-buffer (plist-get logger :buffer) formatted level))
         ((eq handler 'file)
          (when-let ((file (plist-get logger :file)))
            (elog--log-to-file file formatted)))
         ((eq handler 'message)
          (elog--log-to-message formatted))))))
  t)

;;;; Level-Specific Logging Functions

(defun elog-trace (logger format-string &rest args)
  "Log a TRACE level message.
LOGGER is the logger to use.
FORMAT-STRING and ARGS are passed to `format' to create the message."
  (declare (indent defun))
  (elog logger 'trace format-string args))

(defun elog-debug (logger format-string &rest args)
  "Log a DEBUG level message.
LOGGER is the logger to use.
FORMAT-STRING and ARGS are passed to `format' to create the message."
  (declare (indent defun))
  (elog logger 'debug format-string args))

(defun elog-info (logger format-string &rest args)
  "Log an INFO level message.
LOGGER is the logger to use.
FORMAT-STRING and ARGS are passed to `format' to create the message."
  (declare (indent defun))
  (elog logger 'info format-string args))

(defun elog-warning (logger format-string &rest args)
  "Log a WARNING level message.
LOGGER is the logger to use.
FORMAT-STRING and ARGS are passed to `format' to create the message."
  (declare (indent defun))
  (elog logger 'warning format-string args))

(defun elog-error (logger format-string &rest args)
  "Log an ERROR level message.
LOGGER is the logger to use.
FORMAT-STRING and ARGS are passed to `format' to create the message."
  (declare (indent defun))
  (elog logger 'error format-string args))

(defun elog-fatal (logger format-string &rest args)
  "Log a FATAL level message.
LOGGER is the logger to use.
FORMAT-STRING and ARGS are passed to `format' to create the message.
FATAL is the most severe level, indicating critical system failures."
  (declare (indent defun))
  (elog logger 'fatal format-string args))

;;;; Conditional Logging Macros

(defmacro elog-when-trace (logger &rest body)
  "Execute BODY only if TRACE level is enabled for LOGGER.
This can be used to avoid expensive computation for disabled log levels."
  (declare (indent 1))
  `(when (elog--level-enabled-p ,logger 'trace)
     ,@body))

(defmacro elog-when-debug (logger &rest body)
  "Execute BODY only if DEBUG level is enabled for LOGGER."
  (declare (indent 1))
  `(when (elog--level-enabled-p ,logger 'debug)
     ,@body))

(defmacro elog-when-info (logger &rest body)
  "Execute BODY only if INFO level is enabled for LOGGER."
  (declare (indent 1))
  `(when (elog--level-enabled-p ,logger 'info)
     ,@body))

(defmacro elog-when-warning (logger &rest body)
  "Execute BODY only if WARNING level is enabled for LOGGER."
  (declare (indent 1))
  `(when (elog--level-enabled-p ,logger 'warning)
     ,@body))

(defmacro elog-when-error (logger &rest body)
  "Execute BODY only if ERROR level is enabled for LOGGER."
  (declare (indent 1))
  `(when (elog--level-enabled-p ,logger 'error)
     ,@body))

(defmacro elog-when-fatal (logger &rest body)
  "Execute BODY only if FATAL level is enabled for LOGGER."
  (declare (indent 1))
  `(when (elog--level-enabled-p ,logger 'fatal)
     ,@body))

;;;; Context Macros

(defmacro elog-with-context (logger context &rest body)
  "Execute BODY with temporary additional CONTEXT for LOGGER.
CONTEXT should be a plist of key-value pairs.
The context is automatically removed after BODY completes."
  (declare (indent 2))
  (let ((orig-ctx (gensym "orig-ctx"))
        (ctx-alist (gensym "ctx-alist")))
    `(let* ((,orig-ctx (plist-get ,logger :context))
            (,ctx-alist (cl-loop for (k v) on ,context by #'cddr
                                 collect (cons k v))))
       (unwind-protect
           (progn
             (plist-put ,logger :context (append ,ctx-alist ,orig-ctx))
             ,@body)
         (plist-put ,logger :context ,orig-ctx)))))

;;;; Utility Functions

(defun elog-quit ()
  "Quit the elog buffer and restore previous window configuration."
  (interactive)
  (let ((buffer (current-buffer)))
    (quit-window)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (local-unset-key "q")))))

(defun elog-log-view (logger)
  "Switch to LOGGER's log buffer for viewing."
  (interactive)
  (switch-to-buffer
   (get-buffer-create (plist-get logger :buffer)))
  (local-set-key "q" 'elog-quit)
  (goto-char (point-max)))

(defun elog-log-clear (logger)
  "Clear all logs in LOGGER's buffer."
  (interactive)
  (with-current-buffer
      (get-buffer-create (plist-get logger :buffer))
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun elog-level-p (logger level)
  "Check if LEVEL is enabled for LOGGER."
  (elog--level-enabled-p logger level))

(defun elog-get-level (logger)
  "Get the current log level of LOGGER."
  (plist-get logger :level))

(defun elog-get-name (logger)
  "Get the name of LOGGER."
  (plist-get logger :name))

;;;; Exception/Error Logging

(defun elog-exception (logger error-data &optional context)
  "Log an exception/error with ERROR-DATA at ERROR level.
ERROR-DATA should be an error object from `condition-case'.
Optional CONTEXT provides additional information."
  (let ((error-type (car error-data))
        (error-message (error-message-string error-data)))
    (when context
      (elog-add-context logger 'error-context context))
    (elog-error logger "Exception [%s]: %s" error-type error-message)))

(defmacro elog-catch (logger &rest body)
  "Execute BODY and log any errors using LOGGER at ERROR level.
Returns the result of BODY, or nil if an error occurred."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (elog-exception ,logger err)
      nil)))

(provide 'elog)

;;; elog.el ends here
