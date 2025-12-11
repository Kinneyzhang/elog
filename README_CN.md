# elog - Emacs Lisp 强大的日志系统

<p align="center">
  <a href="https://www.gnu.org/licenses/gpl-3.0"><img src="https://img.shields.io/badge/License-GPL%20v3-blue.svg" alt="License: GPL v3"></a>
  <a href="https://www.gnu.org/software/emacs/"><img src="https://img.shields.io/badge/Emacs-26.1%2B-blueviolet.svg" alt="Emacs 26.1+"></a>
  <a href="#"><img src="https://img.shields.io/badge/Version-2.0.0-green.svg" alt="Version 2.0.0"></a>
</p>

[English](README.md) | 中文

**elog** 是一个全面、功能丰富的 Emacs Lisp 日志系统，灵感来自于流行的日志框架如 Log4j (Java)、Winston (Node.js) 和 loguru (Python)。它为您的 Emacs 包和配置提供了结构化、灵活的日志方法。

## ✨ 功能特性

- **多日志级别**: TRACE, DEBUG, INFO, WARNING, ERROR, FATAL - 可配置最低级别过滤
- **命名日志器**: 创建多个具有唯一名称的日志器，用于识别日志来源
- **多输出处理器**: 输出到缓冲区、文件或消息区域（回显区域）
- **可自定义格式模式**: 使用各种占位符定义自己的日志消息格式
- **上下文支持（类似MDC）**: 将上下文数据附加到日志消息，类似于 Log4j 中的 MDC
- **全局和作用域上下文**: 在全局、日志器或代码块级别设置上下文
- **自动缓冲区轮换**: 通过自动修剪行数保持日志缓冲区大小可管理
- **彩色输出**: 使用可自定义的外观区分不同日志级别
- **调用者信息**: 可选包含源函数信息
- **条件日志**: 性能优化的宏，在级别禁用时跳过计算
- **异常日志**: 便捷的函数用于记录错误和异常
- **文件日志**: 持久化日志到文件，自动创建目录

## 📦 安装

### 手动安装

1. 将 `elog.el` 下载到您的 Emacs 加载路径：

```bash
git clone https://github.com/Kinneyzhang/elog.git ~/.emacs.d/site-lisp/elog
```

2. 添加到您的 Emacs 配置：

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/elog")
(require 'elog)
```

### 使用 use-package 和 straight.el

```elisp
(use-package elog
  :straight (:host github :repo "Kinneyzhang/elog"))
```

### 使用 Quelpa

```elisp
(quelpa '(elog :fetcher github :repo "Kinneyzhang/elog"))
```

## 🚀 快速开始

### 基本用法

```elisp
;; 创建日志器
(setq my-logger (elog-logger :name "myapp" :level 'info))

;; 记录不同级别的日志
(elog-info my-logger "应用程序已启动")
(elog-warning my-logger "未找到配置文件，使用默认值")
(elog-error my-logger "连接失败: %s" error-message)

;; 查看日志缓冲区
(elog-log-view my-logger)
```

### 带格式参数

```elisp
(elog-info my-logger "用户 %s 从 %s 登录" username ip-address)
(elog-debug my-logger "处理 %d 个项目 (%.2f%%)" count percentage)
```

### 带上下文

```elisp
(elog-with-context my-logger '(:user-id "12345" :session "abc")
  (elog-info my-logger "开始事务")
  (elog-info my-logger "事务完成"))
```

## 📊 日志级别

Elog 提供六个日志级别，按严重程度从低到高排列：

| 级别 | 优先级 | 描述 | 颜色 |
|------|--------|------|------|
| `trace` | 0 | 细粒度调试信息 | 灰色 |
| `debug` | 1 | 调试信息 | 青色 |
| `info` | 2 | 一般信息消息 | 绿色 |
| `warning` | 3 | 潜在有害情况的警告消息 | 橙色 |
| `error` | 4 | 错误事件的错误消息 | 红色 |
| `fatal` | 5 | 严重故障的严重错误消息 | 红色（粗体，下划线） |

只有当消息级别等于或高于日志器配置的级别时，才会记录消息。

```elisp
;; 设置 'warning 级别的日志器只会输出 warning、error 和 fatal 消息
(setq logger (elog-logger :name "example" :level 'warning))

(elog-debug logger "这不会显示")   ; 低于阈值
(elog-info logger "这也不会显示")  ; 低于阈值
(elog-warning logger "这会显示")   ; 在阈值
(elog-error logger "这会显示")     ; 高于阈值
```

## 🛠️ API 参考

### 创建日志器

#### `elog-logger (&key name level buffer file format handlers context)`

创建一个具有指定配置的新日志器。

**参数：**

| 参数 | 类型 | 默认值 | 描述 |
|------|------|--------|------|
| `:name` | string | `"elog"` | 日志消息中用于识别的日志器名称/标签 |
| `:level` | symbol | `'debug` | 最低日志级别 (`trace`, `debug`, `info`, `warning`, `error`, `fatal`) |
| `:buffer` | string | `"*elog*"` | 缓冲区处理器输出的缓冲区名称 |
| `:file` | string | `nil` | 文件处理器输出的文件路径 |
| `:format` | string | `"%t\| [%l] <%n> %m"` | 自定义格式模式（参见格式模式） |
| `:handlers` | list | `'(buffer)` | 输出处理器列表 (`buffer`, `file`, `message`) |
| `:context` | alist | `nil` | 初始上下文键值对 |

**示例：**

```elisp
;; 简单日志器
(setq my-logger (elog-logger :name "myapp" :level 'info))

;; 带文件输出的日志器
(setq file-logger (elog-logger :name "app"
                               :level 'debug
                               :file "~/logs/app.log"
                               :handlers '(buffer file)))

;; 带自定义格式的日志器
(setq custom-logger (elog-logger :name "api"
                                 :level 'info
                                 :format "%T [%l] %n: %m"))
```

### 日志函数

#### `elog-trace (logger format-string &rest args)`
记录 TRACE 级别消息。

#### `elog-debug (logger format-string &rest args)`
记录 DEBUG 级别消息。

#### `elog-info (logger format-string &rest args)`
记录 INFO 级别消息。

#### `elog-warning (logger format-string &rest args)`
记录 WARNING 级别消息。

#### `elog-error (logger format-string &rest args)`
记录 ERROR 级别消息。

#### `elog-fatal (logger format-string &rest args)`
记录 FATAL 级别消息。

**示例：**

```elisp
(elog-info logger "用户已登录")
(elog-error logger "连接失败: %s" error-msg)
(elog-debug logger "请求: method=%s path=%s" method path)
```

### 日志器配置

#### `elog-set-level (logger level)`
设置日志器的最低日志级别。

#### `elog-set-buffer (logger buffer)`
设置输出缓冲区名称。

#### `elog-set-file (logger file)`
设置输出文件路径。

#### `elog-set-format (logger format)`
设置日志格式模式。

#### `elog-set-handlers (logger handlers)`
设置输出处理器列表。

#### `elog-add-handler (logger handler)`
向现有列表添加处理器。

**示例：**

```elisp
;; 动态更改级别
(elog-set-level my-logger 'debug)

;; 向现有日志器添加文件日志
(elog-set-file my-logger "~/logs/app.log")
(elog-add-handler my-logger 'file)
```

### 上下文管理

#### 日志器上下文

```elisp
;; 设置日志器的上下文
(elog-set-context logger '((user . "john") (session . "abc123")))

;; 添加单个上下文键值
(elog-add-context logger 'request-id "REQ-001")
```

#### 全局上下文

```elisp
;; 设置全局上下文（对所有日志器可用）
(elog-set-global-context 'app-version "2.0.0")
(elog-set-global-context 'environment "production")

;; 删除全局上下文键
(elog-remove-global-context 'app-version)

;; 清除所有全局上下文
(elog-clear-global-context)
```

#### 作用域上下文

```elisp
;; 代码块的临时上下文
(elog-with-context logger '(:transaction-id "TXN-999" :amount 100.00)
  (elog-info logger "开始事务")
  (elog-info logger "处理支付")
  (elog-info logger "事务完成"))
;; 代码块后上下文自动删除
```

### 异常日志

#### `elog-exception (logger error-data &optional context)`
在 ERROR 级别记录异常/错误。

#### `elog-catch (logger &rest body)` (宏)
执行 body 并自动记录任何错误。

**示例：**

```elisp
;; 手动异常日志
(condition-case err
    (risky-operation)
  (error
   (elog-exception my-logger err "处理请求时")))

;; 自动异常捕获和日志
(elog-catch my-logger
  (/ 1 0))  ; 错误将被自动记录
```

### 条件日志宏

这些宏在日志级别禁用时避免昂贵的计算：

```elisp
(elog-when-debug logger
  (elog-debug logger "昂贵的调试信息: %s" 
              (compute-expensive-debug-data)))

(elog-when-trace logger
  (elog-trace logger "非常详细的跟踪: %s"
              (generate-trace-report)))
```

可用宏：`elog-when-trace`、`elog-when-debug`、`elog-when-info`、`elog-when-warning`、`elog-when-error`、`elog-when-fatal`

### 实用函数

#### `elog-log-view (logger)`
切换到日志器的日志缓冲区。

#### `elog-log-clear (logger)`
清除日志器缓冲区中的所有日志。

#### `elog-quit`
退出 elog 缓冲区并恢复窗口配置。

#### `elog-level-p (logger level)`
检查日志器是否启用了某个级别。

#### `elog-get-level (logger)`
获取日志器的当前日志级别。

#### `elog-get-name (logger)`
获取日志器的名称。

## 📝 格式模式

使用以下占位符自定义日志消息格式：

| 占位符 | 描述 | 示例 |
|--------|------|------|
| `%t` | 短时间戳 (HH:MM:SS.mmm) | `14:30:45.123` |
| `%T` | 完整时间戳 (YYYY-MM-DD HH:MM:SS.mmm) | `2024-01-15 14:30:45.123` |
| `%l` | 日志级别（大写） | `INFO` |
| `%n` | 日志器名称 | `myapp` |
| `%m` | 日志消息 | `用户已登录` |
| `%c` | 上下文数据 | `user=john session=abc` |
| `%f` | 源文件名 | `app.el` |
| `%L` | 源行号 | `42` |
| `%F` | 源函数名 | `my-function` |

**格式示例：**

```elisp
;; 默认格式
"%t| [%l] <%n> %m"
;; 输出: 14:30:45.123| [INFO] <myapp> 用户已登录

;; 完整时间戳
"%T [%l] %n - %m"
;; 输出: 2024-01-15 14:30:45.123 [INFO] myapp - 用户已登录

;; 最小格式
"[%l] %m"
;; 输出: [INFO] 用户已登录

;; 带上下文
"%t [%l] %n: %m {%c}"
;; 输出: 14:30:45.123 [INFO] myapp: 用户已登录 {user=john session=abc}

;; JSON 样式格式
"{\"time\":\"%T\",\"level\":\"%l\",\"logger\":\"%n\",\"msg\":\"%m\"}"
```

## ⚙️ 配置变量

| 变量 | 默认值 | 描述 |
|------|--------|------|
| `elog-default-buffer` | `"*elog*"` | 日志输出的默认缓冲区 |
| `elog-default-format` | `"%t\| [%l] <%n> %m"` | 默认格式模式 |
| `elog-max-buffer-lines` | `10000` | 缓冲区最大行数（nil = 无限制） |
| `elog-colorize` | `t` | 在缓冲区中启用彩色输出 |
| `elog-include-caller` | `nil` | 包含调用者函数信息 |

## 🎨 自定义颜色

通过修改外观自定义日志级别颜色：

```elisp
;; 将 INFO 级别改为蓝色
(set-face-attribute 'elog-info-face nil :foreground "blue")

;; 使 WARNING 级别加粗
(set-face-attribute 'elog-warning-face nil :weight 'bold)

;; 自定义 FATAL 外观
(set-face-attribute 'elog-fatal-face nil 
                    :foreground "white"
                    :background "red"
                    :weight 'bold)
```

可用外观：
- `elog-trace-face`
- `elog-debug-face`
- `elog-info-face`
- `elog-warning-face`
- `elog-error-face`
- `elog-fatal-face`

## 📖 示例

### Web 请求日志

```elisp
(setq http-logger (elog-logger :name "http-server"
                               :level 'info
                               :format "%T [%l] %n: %m"))

(elog-info http-logger "收到 GET /api/users 请求")

(elog-with-context http-logger '(:method "GET" 
                                 :path "/api/users"
                                 :client-ip "192.168.1.100")
  (elog-info http-logger "正在验证请求")
  (elog-info http-logger "用户已验证: admin")
  (elog-info http-logger "返回 200 OK，42 个用户"))

(elog-info http-logger "请求完成，耗时 150ms")
```

### 多组件日志器

```elisp
;; 为不同组件创建不同级别的日志器
(setq db-logger (elog-logger :name "database" :level 'debug))
(setq auth-logger (elog-logger :name "auth" :level 'info))
(setq api-logger (elog-logger :name "api" :level 'warning))

(elog-debug db-logger "执行查询: SELECT * FROM users")
(elog-info auth-logger "用户 'admin' 已验证")
(elog-warning api-logger "客户端接近速率限制")
```

### 生产环境文件日志

```elisp
(setq prod-logger (elog-logger :name "production"
                               :level 'warning  ; 仅警告及以上
                               :file "/var/log/myapp/app.log"
                               :format "%T [%l] %n: %m"
                               :handlers '(file)))

;; 为所有日志条目设置全局上下文
(elog-set-global-context 'hostname (system-name))
(elog-set-global-context 'pid (emacs-pid))

(elog-error prod-logger "关键数据库连接丢失")
```

### 完整应用程序设置

```elisp
(defun my-app-init-logging ()
  "初始化我的应用程序日志。"
  ;; 设置全局上下文
  (elog-set-global-context 'app "MyApp")
  (elog-set-global-context 'version "2.0.0")
  
  ;; 创建主应用程序日志器
  (setq my-app-logger 
        (elog-logger :name "myapp"
                     :level (if my-app-debug-mode 'debug 'info)
                     :file (expand-file-name "myapp.log" user-emacs-directory)
                     :handlers '(buffer file)))
  
  (elog-info my-app-logger "应用程序已初始化"))

(defun my-app-shutdown ()
  "关闭应用程序。"
  (elog-info my-app-logger "正在关闭...")
  (elog-clear-global-context))
```

## 🔧 故障排除

### 日志不显示

1. 检查日志级别是否足够高：
   ```elisp
   (elog-get-level my-logger)  ; 返回当前级别
   (elog-level-p my-logger 'debug)  ; 检查 debug 是否启用
   ```

2. 验证处理器已配置：
   ```elisp
   (plist-get my-logger :handlers)
   ```

### 文件日志问题

1. 确保目录存在（elog 会自动创建目录）
2. 检查文件权限
3. 验证文件路径：
   ```elisp
   (plist-get my-logger :file)
   ```

### 缓冲区轮换

如果日志消失，检查 `elog-max-buffer-lines`：
```elisp
(setq elog-max-buffer-lines nil)  ; 禁用轮换
```

## 🤝 贡献

欢迎贡献！请随时提交问题和拉取请求。

1. Fork 仓库
2. 创建功能分支 (`git checkout -b feature/amazing-feature`)
3. 提交更改 (`git commit -m '添加一些很棒的功能'`)
4. 推送到分支 (`git push origin feature/amazing-feature`)
5. 打开拉取请求

## 📄 许可证

本项目根据 GNU 通用公共许可证 v3.0 许可 - 详见 [LICENSE](LICENSE) 文件。

## 👤 作者

**Kinney Zhang**

- GitHub: [@Kinneyzhang](https://github.com/Kinneyzhang)

## 🙏 致谢

- 灵感来自 [Log4j](https://logging.apache.org/log4j/) (Java)
- 灵感来自 [Winston](https://github.com/winstonjs/winston) (Node.js)
- 灵感来自 [loguru](https://github.com/Delgan/loguru) (Python)

---

<p align="center">用 ❤️ 为 Emacs 社区制作</p>
