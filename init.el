;;; init.el -- 初始化配置
;;; Commentary:
;;; Code:

;; 添加加载路径
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")

;; 显示启动时间
(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

;; 显示每个包的加载时间
(require 'benchmark)
(let ((lisp-dir "~/.emacs.d/lisp"))
  (add-to-list 'load-path lisp-dir)
  (mapc (lambda (fname)
          (let ((feat (intern (file-name-base fname))))
            (message "Feature '%s' loaded in %.2fs" feat
                     (benchmark-elapse (require feat fname)))))
        (directory-files lisp-dir t "\\.el")))

;; 加载各个模块配置
(require 'init-package)        ;; 包管理器 - 管理elisp包的下载、更新和加载
(require 'init-custom)         ;; 基础设置 - 默认编码、界面、操作习惯等基本配置
(require 'init-theme)          ;; 主题美化 - 字体、配色、模式栏等外观配置
(require 'init-autocomplete)   ;; 自动补全 - company、ivy等补全框架
(require 'init-errors)         ;; 错误检查 - flycheck、flymake等语法检查工具
(require 'init-project)        ;; 项目管理 - projectile等项目管理工具
(require 'init-magit)          ;; Git集成 - magit版本控制工具
(require 'init-dashboard)      ;; 启动页面 - 美化的emacs启动界面
(require 'init-ide)            ;; IDE环境 - LSP、eglot等开发环境支持
(require 'init-org)            ;; Org工具 - 知识管理、文档编辑等org-mode配置
(require 'init-hydra)          ;; 快捷键组 - 便捷的快捷键管理和操作
(require 'init-hooks)          ;; 钩子函数 - 各种模式的钩子和自定义函数

;;; init.el ends here
