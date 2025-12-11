;;; -*- lexical-binding: t; -*-

;;; elog-examples.el --- Usage examples for elog logging system

;; This file contains examples demonstrating all features of elog.
;; Load elog.el first, then evaluate these examples in Emacs.

;;; Usage:
;; 1. Load elog: (load-file "elog.el")
;; 2. Evaluate any example below

;;; Code:

(require 'elog)

;;; ============================================================
;;; Example 1: Basic Logging
;;; ============================================================

;; Create a simple logger with default settings
(defun elog-example-basic ()
  "Demonstrate basic logging functionality."
  (interactive)
  (let ((logger (elog-logger :name "basic-example" :level 'debug)))
    ;; Log messages at different levels
    (elog-trace logger "This is a TRACE message (won't show at debug level)")
    (elog-debug logger "This is a DEBUG message")
    (elog-info logger "This is an INFO message")
    (elog-warning logger "This is a WARNING message")
    (elog-error logger "This is an ERROR message")
    (elog-fatal logger "This is a FATAL message")
    
    ;; View the log buffer
    (elog-log-view logger)))

;;; ============================================================
;;; Example 2: Logging with Format Arguments
;;; ============================================================

(defun elog-example-format-args ()
  "Demonstrate logging with format arguments."
  (interactive)
  (let ((logger (elog-logger :name "format-example" :level 'info))
        (username "alice")
        (user-id 12345)
        (elapsed-time 3.14159))
    
    ;; Use format specifiers like printf/format
    (elog-info logger "User %s logged in" username)
    (elog-info logger "User ID: %d" user-id)
    (elog-info logger "Operation completed in %.2f seconds" elapsed-time)
    (elog-warning logger "Failed login attempt for user %s (attempt #%d)" username 3)
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 3: Log Level Filtering
;;; ============================================================

(defun elog-example-level-filtering ()
  "Demonstrate log level filtering."
  (interactive)
  (let ((logger (elog-logger :name "filter-example" :level 'warning)))
    
    ;; Only WARNING and above will be logged
    (elog-debug logger "DEBUG: This won't appear")
    (elog-info logger "INFO: This won't appear either")
    (elog-warning logger "WARNING: This will appear")
    (elog-error logger "ERROR: This will appear too")
    (elog-fatal logger "FATAL: This will definitely appear")
    
    ;; Change level dynamically
    (elog-set-level logger 'debug)
    (elog-debug logger "DEBUG: Now this will appear after level change")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 4: Custom Format Patterns
;;; ============================================================

(defun elog-example-custom-format ()
  "Demonstrate custom format patterns."
  (interactive)
  
  ;; Format with short timestamp (default)
  (let ((logger1 (elog-logger :name "app1" 
                              :level 'info
                              :format "%t| [%l] <%n> %m")))
    (elog-info logger1 "Default format with short timestamp"))
  
  ;; Format with full timestamp
  (let ((logger2 (elog-logger :name "app2"
                              :level 'info
                              :format "%T [%l] %n - %m")))
    (elog-info logger2 "Full timestamp format"))
  
  ;; Minimal format
  (let ((logger3 (elog-logger :name "app3"
                              :level 'info
                              :format "[%l] %m")))
    (elog-info logger3 "Minimal format - just level and message"))
  
  ;; JSON-like format
  (let ((logger4 (elog-logger :name "app4"
                              :level 'info
                              :format "{\"time\":\"%T\",\"level\":\"%l\",\"logger\":\"%n\",\"msg\":\"%m\"}")))
    (elog-info logger4 "JSON-style log entry"))
  
  (switch-to-buffer "*elog*"))

;;; ============================================================
;;; Example 5: File Logging
;;; ============================================================

(defun elog-example-file-logging ()
  "Demonstrate logging to files."
  (interactive)
  (let* ((log-file (expand-file-name "~/elog-test.log"))
         (logger (elog-logger :name "file-logger"
                              :level 'info
                              :file log-file
                              :handlers '(buffer file))))
    
    ;; Log messages go to both buffer and file
    (elog-info logger "Application started")
    (elog-info logger "Connecting to database...")
    (elog-warning logger "Connection slow, retrying...")
    (elog-info logger "Connected successfully")
    (elog-error logger "Query failed: timeout")
    
    ;; Show the log file content
    (message "Log file created at: %s" log-file)
    (message "File contents:\n%s" 
             (with-temp-buffer
               (insert-file-contents log-file)
               (buffer-string)))
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 6: Multiple Handlers
;;; ============================================================

(defun elog-example-multiple-handlers ()
  "Demonstrate using multiple output handlers."
  (interactive)
  (let ((logger (elog-logger :name "multi-handler"
                             :level 'info
                             :file (expand-file-name "~/multi-test.log")
                             :handlers '(buffer file message))))
    
    ;; This message goes to:
    ;; 1. *elog* buffer
    ;; 2. ~/multi-test.log file
    ;; 3. Echo area (message)
    (elog-info logger "This message appears in buffer, file, AND echo area!")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 7: Context (MDC-like functionality)
;;; ============================================================

(defun elog-example-context ()
  "Demonstrate context/MDC functionality."
  (interactive)
  (let ((logger (elog-logger :name "context-demo"
                             :level 'info
                             :format "%t| [%l] %n: %m {%c}")))
    
    ;; Add context to logger
    (elog-add-context logger 'request-id "REQ-12345")
    (elog-add-context logger 'user-id "USER-789")
    
    (elog-info logger "Processing request")
    (elog-info logger "Validating input")
    (elog-info logger "Request completed")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 8: Global Context
;;; ============================================================

(defun elog-example-global-context ()
  "Demonstrate global context shared across loggers."
  (interactive)
  
  ;; Set global context (available to all loggers)
  (elog-set-global-context 'app-name "MyApplication")
  (elog-set-global-context 'app-version "2.0.0")
  (elog-set-global-context 'environment "production")
  
  (let ((logger1 (elog-logger :name "service-A"
                              :level 'info
                              :format "%t| [%l] %n: %m {%c}"))
        (logger2 (elog-logger :name "service-B"
                              :level 'info
                              :format "%t| [%l] %n: %m {%c}")))
    
    ;; Both loggers will include global context
    (elog-info logger1 "Service A started")
    (elog-info logger2 "Service B started")
    
    ;; Clean up global context
    (elog-clear-global-context)
    
    (elog-info logger1 "After clearing global context"))
  
  (switch-to-buffer "*elog*"))

;;; ============================================================
;;; Example 9: Scoped Context with elog-with-context
;;; ============================================================

(defun elog-example-scoped-context ()
  "Demonstrate scoped context using elog-with-context macro."
  (interactive)
  (let ((logger (elog-logger :name "scoped-ctx"
                             :level 'info
                             :format "%t| [%l] %n: %m {%c}")))
    
    (elog-info logger "Before scoped context")
    
    ;; Context is only active within this block
    (elog-with-context logger '(:transaction-id "TXN-999" :customer "ACME Corp")
      (elog-info logger "Starting transaction")
      (elog-info logger "Processing payment")
      (elog-info logger "Transaction complete"))
    
    ;; Context is automatically removed after the block
    (elog-info logger "After scoped context - no transaction info")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 10: Exception Logging
;;; ============================================================

(defun elog-example-exception-logging ()
  "Demonstrate exception/error logging."
  (interactive)
  (let ((logger (elog-logger :name "exception-demo" :level 'info)))
    
    (elog-info logger "Starting risky operation...")
    
    ;; elog-catch automatically logs any errors
    (elog-catch logger
      (/ 1 0))  ; This will cause an error
    
    ;; Manually log an exception with context
    (condition-case err
        (error "Custom error message")
      (error
       (elog-exception logger err "While processing user request")))
    
    (elog-info logger "Continuing after error handling")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 11: Conditional Logging (Performance Optimization)
;;; ============================================================

(defun elog-example-conditional-logging ()
  "Demonstrate conditional logging for performance."
  (interactive)
  (let ((logger (elog-logger :name "conditional" :level 'warning))
        (expensive-computation-count 0))
    
    ;; This expensive computation will NOT run because debug is disabled
    (elog-when-debug logger
      (setq expensive-computation-count (1+ expensive-computation-count))
      (elog-debug logger "Expensive debug info: %s" 
                  (cl-loop for i from 1 to 1000 collect i)))
    
    ;; This WILL run because warning is enabled
    (elog-when-warning logger
      (setq expensive-computation-count (1+ expensive-computation-count))
      (elog-warning logger "Warning level check passed"))
    
    (message "Expensive computations executed: %d (should be 1)" 
             expensive-computation-count)
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 12: Buffer Rotation
;;; ============================================================

(defun elog-example-buffer-rotation ()
  "Demonstrate automatic buffer rotation."
  (interactive)
  (let ((elog-max-buffer-lines 5)  ; Keep only 5 lines
        (logger (elog-logger :name "rotation-demo" :level 'info)))
    
    ;; Log 10 messages - only last 5 will remain
    (dotimes (i 10)
      (elog-info logger "Log message number %d" (1+ i)))
    
    (message "Logged 10 messages, but buffer only contains last 5")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 13: Colored Output
;;; ============================================================

(defun elog-example-colored-output ()
  "Demonstrate colored log output."
  (interactive)
  (let ((elog-colorize t)  ; Enable colors (default)
        (logger (elog-logger :name "color-demo" :level 'trace)))
    
    ;; Each level has a different color
    (elog-trace logger "TRACE - Gray color")
    (elog-debug logger "DEBUG - Cyan color")
    (elog-info logger "INFO - Green color")
    (elog-warning logger "WARNING - Orange color")
    (elog-error logger "ERROR - Red color")
    (elog-fatal logger "FATAL - Red bold underlined")
    
    (elog-log-view logger)))

;;; ============================================================
;;; Example 14: Real-World Web Request Logging
;;; ============================================================

(defun elog-example-web-request ()
  "Simulate a web request logging scenario."
  (interactive)
  (let ((request-logger (elog-logger :name "http-server"
                                     :level 'info
                                     :format "%T [%l] %n: %m")))
    
    ;; Simulate incoming request
    (elog-info request-logger "Received GET /api/users")
    
    (elog-with-context request-logger '(:method "GET" 
                                        :path "/api/users"
                                        :client-ip "192.168.1.100")
      (elog-info request-logger "Authenticating request")
      (elog-info request-logger "User authenticated: admin")
      (elog-info request-logger "Querying database")
      (elog-info request-logger "Returning 200 OK with 42 users"))
    
    (elog-info request-logger "Request completed in 150ms")
    
    (elog-log-view request-logger)))

;;; ============================================================
;;; Example 15: Multiple Loggers for Different Components
;;; ============================================================

(defun elog-example-multiple-loggers ()
  "Demonstrate using multiple loggers for different components."
  (interactive)
  
  ;; Create separate loggers for different components
  (let ((db-logger (elog-logger :name "database" 
                                :level 'debug
                                :format "%t [%l] [%n] %m"))
        (auth-logger (elog-logger :name "auth"
                                  :level 'info
                                  :format "%t [%l] [%n] %m"))
        (api-logger (elog-logger :name "api"
                                 :level 'warning
                                 :format "%t [%l] [%n] %m")))
    
    ;; Database logs (debug and above)
    (elog-debug db-logger "Opening connection pool")
    (elog-debug db-logger "Executing: SELECT * FROM users")
    (elog-info db-logger "Query returned 100 rows")
    
    ;; Auth logs (info and above)
    (elog-debug auth-logger "This won't show - below info level")
    (elog-info auth-logger "User 'admin' login successful")
    (elog-warning auth-logger "Session about to expire")
    
    ;; API logs (warning and above only)
    (elog-debug api-logger "This won't show")
    (elog-info api-logger "This won't show either")
    (elog-warning api-logger "Rate limit approaching for client")
    (elog-error api-logger "Rate limit exceeded!")
    
    (switch-to-buffer "*elog*")))

;;; ============================================================
;;; Example 16: Complete Application Simulation
;;; ============================================================

(defun elog-example-full-application ()
  "Simulate a complete application with comprehensive logging."
  (interactive)
  
  ;; Clear previous logs
  (when (get-buffer "*elog*")
    (with-current-buffer "*elog*"
      (let ((inhibit-read-only t))
        (erase-buffer))))
  
  ;; Set global application context
  (elog-set-global-context 'app "MyApp")
  (elog-set-global-context 'version "2.0.0")
  
  (let ((app-logger (elog-logger :name "app"
                                 :level 'info
                                 :format "%T [%l] <%n> %m")))
    
    ;; Application startup
    (elog-info app-logger "=== Application Starting ===")
    (elog-info app-logger "Loading configuration...")
    (elog-info app-logger "Initializing database connection...")
    
    ;; Simulate some operations
    (elog-with-context app-logger '(:operation "user-import")
      (elog-info app-logger "Starting user import job")
      (elog-info app-logger "Processing batch 1/3")
      (elog-info app-logger "Processing batch 2/3")
      (elog-warning app-logger "Batch 3 contains duplicate entries")
      (elog-info app-logger "Import completed with warnings"))
    
    ;; Simulate an error scenario
    (elog-catch app-logger
      (when t
        (error "Simulated database connection lost")))
    
    ;; Application shutdown
    (elog-info app-logger "Shutting down gracefully...")
    (elog-info app-logger "=== Application Stopped ===")
    
    ;; Cleanup
    (elog-clear-global-context))
  
  (switch-to-buffer "*elog*"))

;;; ============================================================
;;; Run All Examples
;;; ============================================================

(defun elog-run-all-examples ()
  "Run all elog examples in sequence."
  (interactive)
  (message "Running all elog examples...")
  
  ;; Clear buffer first
  (when (get-buffer "*elog*")
    (with-current-buffer "*elog*"
      (let ((inhibit-read-only t))
        (erase-buffer))))
  
  (elog-example-basic)
  (sit-for 0.5)
  (elog-example-format-args)
  (sit-for 0.5)
  (elog-example-level-filtering)
  (sit-for 0.5)
  (elog-example-context)
  (sit-for 0.5)
  (elog-example-colored-output)
  
  (message "Examples completed! Check the *elog* buffer for output."))

(provide 'elog-examples)

;;; elog-examples.el ends here
