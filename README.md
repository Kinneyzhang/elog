# elog - A Powerful Logging System for Emacs Lisp

<p align="center">
  <a href="https://www.gnu.org/licenses/gpl-3.0"><img src="https://img.shields.io/badge/License-GPL%20v3-blue.svg" alt="License: GPL v3"></a>
  <a href="https://www.gnu.org/software/emacs/"><img src="https://img.shields.io/badge/Emacs-26.1%2B-blueviolet.svg" alt="Emacs 26.1+"></a>
  <a href="#"><img src="https://img.shields.io/badge/Version-2.0.0-green.svg" alt="Version 2.0.0"></a>
</p>

English | [中文](README_CN.md)

**elog** is a comprehensive, feature-rich logging system for Emacs Lisp, inspired by popular logging frameworks like Log4j (Java), Winston (Node.js), and loguru (Python). It provides a structured, flexible approach to logging in your Emacs packages and configurations.

## ✨ Features

- **Multiple Log Levels**: TRACE, DEBUG, INFO, WARNING, ERROR, FATAL - with configurable minimum level filtering
- **Named Loggers**: Create multiple loggers with unique names for identifying log sources
- **Multiple Output Handlers**: Output to buffers, files, or the message area (echo area)
- **Customizable Format Patterns**: Define your own log message format with various placeholders
- **Context Support (MDC-like)**: Attach contextual data to log messages, similar to MDC in Log4j
- **Global & Scoped Context**: Set context at global, logger, or block level
- **Automatic Buffer Rotation**: Keep log buffer size manageable with automatic line pruning
- **Colored Output**: Visual distinction between log levels with customizable faces
- **Caller Information**: Optional inclusion of source function information
- **Conditional Logging**: Performance-optimized macros that skip evaluation when level is disabled
- **Exception Logging**: Convenient functions for logging errors and exceptions
- **File Logging**: Persistent logging to files with automatic directory creation

## 📦 Installation

### Manual Installation

1. Download `elog.el` to your Emacs load path:

```bash
git clone https://github.com/Kinneyzhang/elog.git ~/.emacs.d/site-lisp/elog
```

2. Add to your Emacs configuration:

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/elog")
(require 'elog)
```

### Using use-package with straight.el

```elisp
(use-package elog
  :straight (:host github :repo "Kinneyzhang/elog"))
```

### Using Quelpa

```elisp
(quelpa '(elog :fetcher github :repo "Kinneyzhang/elog"))
```

## 🚀 Quick Start

### Basic Usage

```elisp
;; Create a logger
(setq my-logger (elog-logger :name "myapp" :level 'info))

;; Log messages at different levels
(elog-info my-logger "Application started")
(elog-warning my-logger "Configuration file not found, using defaults")
(elog-error my-logger "Failed to connect: %s" error-message)

;; View the log buffer
(elog-log-view my-logger)
```

### With Format Arguments

```elisp
(elog-info my-logger "User %s logged in from %s" username ip-address)
(elog-debug my-logger "Processing %d items (%.2f%%)" count percentage)
```

### With Context

```elisp
(elog-with-context my-logger '(:user-id "12345" :session "abc")
  (elog-info my-logger "Starting transaction")
  (elog-info my-logger "Transaction completed"))
```

## 📊 Log Levels

Elog provides six log levels, ordered from lowest to highest severity:

| Level | Priority | Description | Color |
|-------|----------|-------------|-------|
| `trace` | 0 | Fine-grained debugging information | Gray |
| `debug` | 1 | Debugging information | Cyan |
| `info` | 2 | General informational messages | Green |
| `warning` | 3 | Warning messages for potentially harmful situations | Orange |
| `error` | 4 | Error messages for error events | Red |
| `fatal` | 5 | Severe error messages for critical failures | Red (bold, underlined) |

Messages are only logged if their level is equal to or higher than the logger's configured level.

```elisp
;; Logger with 'warning level will only output warning, error, and fatal messages
(setq logger (elog-logger :name "example" :level 'warning))

(elog-debug logger "This won't appear")   ; Below threshold
(elog-info logger "This won't appear")    ; Below threshold
(elog-warning logger "This WILL appear")  ; At threshold
(elog-error logger "This WILL appear")    ; Above threshold
```

## 🛠️ API Reference

### Creating Loggers

#### `elog-logger (&key name level buffer file format handlers context)`

Create a new logger with the specified configuration.

**Parameters:**

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `:name` | string | `"elog"` | Logger name/tag for identification in log messages |
| `:level` | symbol | `'debug` | Minimum log level (`trace`, `debug`, `info`, `warning`, `error`, `fatal`) |
| `:buffer` | string | `"*elog*"` | Buffer name for buffer handler output |
| `:file` | string | `nil` | File path for file handler output |
| `:format` | string | `"%t\| [%l] <%n> %m"` | Custom format pattern (see Format Patterns) |
| `:handlers` | list | `'(buffer)` | List of output handlers (`buffer`, `file`, `message`) |
| `:context` | alist | `nil` | Initial context key-value pairs |

**Example:**

```elisp
;; Simple logger
(setq my-logger (elog-logger :name "myapp" :level 'info))

;; Logger with file output
(setq file-logger (elog-logger :name "app"
                               :level 'debug
                               :file "~/logs/app.log"
                               :handlers '(buffer file)))

;; Logger with custom format
(setq custom-logger (elog-logger :name "api"
                                 :level 'info
                                 :format "%T [%l] %n: %m"))
```

### Logging Functions

#### `elog-trace (logger format-string &rest args)`
Log a TRACE level message.

#### `elog-debug (logger format-string &rest args)`
Log a DEBUG level message.

#### `elog-info (logger format-string &rest args)`
Log an INFO level message.

#### `elog-warning (logger format-string &rest args)`
Log a WARNING level message.

#### `elog-error (logger format-string &rest args)`
Log an ERROR level message.

#### `elog-fatal (logger format-string &rest args)`
Log a FATAL level message.

**Example:**

```elisp
(elog-info logger "User logged in")
(elog-error logger "Connection failed: %s" error-msg)
(elog-debug logger "Request: method=%s path=%s" method path)
```

### Logger Configuration

#### `elog-set-level (logger level)`
Set the minimum log level for the logger.

#### `elog-set-buffer (logger buffer)`
Set the output buffer name.

#### `elog-set-file (logger file)`
Set the output file path.

#### `elog-set-format (logger format)`
Set the log format pattern.

#### `elog-set-handlers (logger handlers)`
Set the list of output handlers.

#### `elog-add-handler (logger handler)`
Add a handler to the existing list.

**Example:**

```elisp
;; Change level dynamically
(elog-set-level my-logger 'debug)

;; Add file logging to existing logger
(elog-set-file my-logger "~/logs/app.log")
(elog-add-handler my-logger 'file)
```

### Context Management

#### Logger Context

```elisp
;; Set context for a logger
(elog-set-context logger '((user . "john") (session . "abc123")))

;; Add a single context key-value
(elog-add-context logger 'request-id "REQ-001")
```

#### Global Context

```elisp
;; Set global context (available to all loggers)
(elog-set-global-context 'app-version "2.0.0")
(elog-set-global-context 'environment "production")

;; Remove a global context key
(elog-remove-global-context 'app-version)

;; Clear all global context
(elog-clear-global-context)
```

#### Scoped Context

```elisp
;; Temporary context for a code block
(elog-with-context logger '(:transaction-id "TXN-999" :amount 100.00)
  (elog-info logger "Starting transaction")
  (elog-info logger "Processing payment")
  (elog-info logger "Transaction complete"))
;; Context is automatically removed after the block
```

### Exception Logging

#### `elog-exception (logger error-data &optional context)`
Log an exception/error at ERROR level.

#### `elog-catch (logger &rest body)` (macro)
Execute body and automatically log any errors.

**Example:**

```elisp
;; Manual exception logging
(condition-case err
    (risky-operation)
  (error
   (elog-exception my-logger err "While processing request")))

;; Automatic exception catching and logging
(elog-catch my-logger
  (/ 1 0))  ; Error will be logged automatically
```

### Conditional Logging Macros

These macros avoid expensive computations when the log level is disabled:

```elisp
(elog-when-debug logger
  (elog-debug logger "Expensive debug info: %s" 
              (compute-expensive-debug-data)))

(elog-when-trace logger
  (elog-trace logger "Very detailed trace: %s"
              (generate-trace-report)))
```

Available macros: `elog-when-trace`, `elog-when-debug`, `elog-when-info`, `elog-when-warning`, `elog-when-error`, `elog-when-fatal`

### Utility Functions

#### `elog-log-view (logger)`
Switch to the logger's log buffer.

#### `elog-log-clear (logger)`
Clear all logs in the logger's buffer.

#### `elog-quit`
Quit the elog buffer and restore window configuration.

#### `elog-level-p (logger level)`
Check if a level is enabled for the logger.

#### `elog-get-level (logger)`
Get the current log level of the logger.

#### `elog-get-name (logger)`
Get the name of the logger.

## 📝 Format Patterns

Customize log message format using these placeholders:

| Placeholder | Description | Example |
|-------------|-------------|---------|
| `%t` | Short timestamp (HH:MM:SS.mmm) | `14:30:45.123` |
| `%T` | Full timestamp (YYYY-MM-DD HH:MM:SS.mmm) | `2024-01-15 14:30:45.123` |
| `%l` | Log level (uppercase) | `INFO` |
| `%n` | Logger name | `myapp` |
| `%m` | Log message | `User logged in` |
| `%c` | Context data | `user=john session=abc` |
| `%f` | Source file name | `app.el` |
| `%L` | Source line number | `42` |
| `%F` | Source function name | `my-function` |

**Format Examples:**

```elisp
;; Default format
"%t| [%l] <%n> %m"
;; Output: 14:30:45.123| [INFO] <myapp> User logged in

;; Full timestamp
"%T [%l] %n - %m"
;; Output: 2024-01-15 14:30:45.123 [INFO] myapp - User logged in

;; Minimal format
"[%l] %m"
;; Output: [INFO] User logged in

;; With context
"%t [%l] %n: %m {%c}"
;; Output: 14:30:45.123 [INFO] myapp: User logged in {user=john session=abc}

;; JSON-like format
"{\"time\":\"%T\",\"level\":\"%l\",\"logger\":\"%n\",\"msg\":\"%m\"}"
```

## ⚙️ Configuration Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `elog-default-buffer` | `"*elog*"` | Default buffer for log output |
| `elog-default-format` | `"%t\| [%l] <%n> %m"` | Default format pattern |
| `elog-max-buffer-lines` | `10000` | Max lines in buffer (nil = unlimited) |
| `elog-colorize` | `t` | Enable colored output in buffers |
| `elog-include-caller` | `nil` | Include caller function information |

## 🎨 Customizing Colors

Customize log level colors by modifying faces:

```elisp
;; Change INFO level to blue
(set-face-attribute 'elog-info-face nil :foreground "blue")

;; Make WARNING level bold
(set-face-attribute 'elog-warning-face nil :weight 'bold)

;; Custom FATAL appearance
(set-face-attribute 'elog-fatal-face nil 
                    :foreground "white"
                    :background "red"
                    :weight 'bold)
```

Available faces:
- `elog-trace-face`
- `elog-debug-face`
- `elog-info-face`
- `elog-warning-face`
- `elog-error-face`
- `elog-fatal-face`

## 📖 Examples

### Web Request Logging

```elisp
(setq http-logger (elog-logger :name "http-server"
                               :level 'info
                               :format "%T [%l] %n: %m"))

(elog-info http-logger "Received GET /api/users")

(elog-with-context http-logger '(:method "GET" 
                                 :path "/api/users"
                                 :client-ip "192.168.1.100")
  (elog-info http-logger "Authenticating request")
  (elog-info http-logger "User authenticated: admin")
  (elog-info http-logger "Returning 200 OK with 42 users"))

(elog-info http-logger "Request completed in 150ms")
```

### Multiple Loggers for Components

```elisp
;; Different loggers for different components with different levels
(setq db-logger (elog-logger :name "database" :level 'debug))
(setq auth-logger (elog-logger :name "auth" :level 'info))
(setq api-logger (elog-logger :name "api" :level 'warning))

(elog-debug db-logger "Executing query: SELECT * FROM users")
(elog-info auth-logger "User 'admin' authenticated")
(elog-warning api-logger "Rate limit approaching for client")
```

### Production Logging with File Output

```elisp
(setq prod-logger (elog-logger :name "production"
                               :level 'warning  ; Only warnings and above
                               :file "/var/log/myapp/app.log"
                               :format "%T [%l] %n: %m"
                               :handlers '(file)))

;; Set global context for all log entries
(elog-set-global-context 'hostname (system-name))
(elog-set-global-context 'pid (emacs-pid))

(elog-error prod-logger "Critical database connection lost")
```

### Complete Application Setup

```elisp
(defun my-app-init-logging ()
  "Initialize logging for my application."
  ;; Set global context
  (elog-set-global-context 'app "MyApp")
  (elog-set-global-context 'version "2.0.0")
  
  ;; Create main application logger
  (setq my-app-logger 
        (elog-logger :name "myapp"
                     :level (if my-app-debug-mode 'debug 'info)
                     :file (expand-file-name "myapp.log" user-emacs-directory)
                     :handlers '(buffer file)))
  
  (elog-info my-app-logger "Application initialized"))

(defun my-app-shutdown ()
  "Shutdown the application."
  (elog-info my-app-logger "Shutting down...")
  (elog-clear-global-context))
```

## 🔧 Troubleshooting

### Logs Not Appearing

1. Check that the log level is high enough:
   ```elisp
   (elog-get-level my-logger)  ; Returns current level
   (elog-level-p my-logger 'debug)  ; Check if debug is enabled
   ```

2. Verify handlers are configured:
   ```elisp
   (plist-get my-logger :handlers)
   ```

### File Logging Issues

1. Ensure the directory exists (elog creates directories automatically)
2. Check file permissions
3. Verify the file path:
   ```elisp
   (plist-get my-logger :file)
   ```

### Buffer Rotation

If logs are disappearing, check `elog-max-buffer-lines`:
```elisp
(setq elog-max-buffer-lines nil)  ; Disable rotation
```

## 🤝 Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the GNU General Public License v3.0 - see the [LICENSE](LICENSE) file for details.

## 👤 Author

**Kinney Zhang**

- GitHub: [@Kinneyzhang](https://github.com/Kinneyzhang)

## 🙏 Acknowledgments

- Inspired by [Log4j](https://logging.apache.org/log4j/) (Java)
- Inspired by [Winston](https://github.com/winstonjs/winston) (Node.js)
- Inspired by [loguru](https://github.com/Delgan/loguru) (Python)

---

<p align="center">Made with ❤️ for the Emacs community</p>
