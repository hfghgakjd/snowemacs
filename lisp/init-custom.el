;;; init-custom.el --- Emacs基础界面与性能优化配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 本模块提供Emacs的基础界面定制和行为配置
;;
;; 设计思路:
;; - 提供清爽专注的编辑界面
;; - 优化默认行为使其更符合现代编辑器习惯
;; - 统一跨平台的字体和编码配置
;;
;; 使用场景:
;; - 日常编程开发
;; - 文档编辑处理
;;
;; 注意事项:
;; - 字体配置需要系统预先安装相应字体
;; - 部分配置依赖外部程序(如sqlite3)
;;
;; 依赖关系:
;; - 需要安装指定的中英文字体
;; - Windows环境下需配置sqlite3路径

;;; Code:

(tool-bar-mode -1)                               ; 关闭工具栏，保持界面简洁
(menu-bar-mode -1)                               ; 关闭菜单栏，最大化编辑区域
(global-display-line-numbers-mode 1)             ; 显示行号，便于代码导航
(if (fboundp 'scroll-bar-mode) 
    (scroll-bar-mode -1))                        ; 关闭滚动条，节省显示空间

;; GC优化配置
(setq gc-cons-threshold (* 50 1000 1000))         ; 提高GC阈值到50MB，减少GC频率
(setq garbage-collection-messages t)              ; 显示GC信息，便于调试

;; 文件加载优化
(setq file-name-handler-alist-original file-name-handler-alist
      file-name-handler-alist nil)               ; 启动时禁用文件处理器

;; 恢复文件处理器（启动完成后执行）
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-original
                  gc-cons-threshold (* 2 1000 1000)))) ; 启动后降低GC阈值到2MB

;; 本地变量优化
(setq-default bidi-display-reordering nil        ; 禁用双向文本重排，提升显示性能
              bidi-paragraph-direction 'left-to-right
              cursor-in-non-selected-windows nil) ; 非选中窗口不显示光标

;; 模块加载优化
(setq idle-update-delay 1.0                      ; 延迟空闲更新时间
      inhibit-compacting-font-caches t)          ; 禁用字体缓存压缩

;; 启动性能分析（可选，用于调试）
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs启动耗时: %s，携带%d个垃圾收集"
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; 滚动优化配置
(setq-default scroll-step 1                      ; 平滑滚动，每次滚动1行
              scroll-margin 0                     ; 边缘滚动范围为0
              scroll-conservatively 10000)        ; 防止跳跃滚动

(electric-pair-mode 1)                           ; 自动配对括号，提高编码效率

;; 文件备份配置
(setq auto-save-mode nil                         ; 关闭自动保存，避免干扰
      make-backup-files nil)                     ; 禁用备份文件，保持目录整洁

;; 窗口配置
(custom-set-variables
 '(initial-frame-alist 
   (quote ((fullscreen . maximized)))))          ; 启动时自动全屏

;; 字体配置
(cond
 ;; 如果是windows系统, 设置Consolas字体
 ((eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-11"))
  ;; 如果是Linux系统
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Ubuntu Mono-12"))
 )
 ;; 设置中文字体
(dolist (charset '(kana han symbol cjk-misc bopomofo))
   (set-fontset-font (frame-parameter nil 'font) charset
                     (font-spec :family "仿宋" :size 18)))

;; 编码配置
(set-language-environment "UTF-8")               ; 设置UTF-8为默认环境
(set-default-coding-systems 'utf-8)              ; 使用UTF-8作为默认编码

;; 空白字符显示配置
(global-whitespace-mode t)                       ; 显示空白字符
(setq whitespace-line-column 2000)               ; 设置超长行阈值

;; 交互优化
(defalias 'yes-or-no-p 'y-or-n-p)               ; 简化确认操作为y/n
(delete-selection-mode 1)                        ; 选中文本直接替换
(cua-selection-mode 1)                           ; 启用CUA选择模式
(setq inhibit-splash-screen 1)                   ; 关闭启动屏幕

;; 快捷键配置
(global-set-key (kbd "C-,") 'set-mark-command)  ; 重映射标记命令到更方便的键位

;; 环境配置
(setq package-check-signature nil)
(when (eq system-type 'windows-nt)
  (setq exec-path (append exec-path '("C:/softs/sqlite3"))))
(setenv "PATH" (concat "C:/softs/sqlite3;" (getenv "PATH")))
(setq debug-on-error t)

(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))
(setq magit-process-connection-type nil)

(provide 'init-custom)
;;; init-custom.el ends here
