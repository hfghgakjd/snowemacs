;;; init.el -- 初始化配置
;;; Commentary:
;;; Code:
(add-to-list 'load-path "~/.emacs.d/org-mode/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp")


;; Display the total loading time in the minibuffer
(defun display-startup-echo-area-message ()
  "Display startup echo area message."
  (message "Initialized in %s" (emacs-init-time)))

;; Always show loading time for each package
(require 'benchmark)
(let ((lisp-dir "~/.emacs.d/lisp"))
  (add-to-list 'load-path lisp-dir)
  (mapc (lambda (fname)
          (let ((feat (intern (file-name-base fname))))
            (message "Feature '%s' loaded in %.2fs" feat
                     (benchmark-elapse (require feat fname)))))
        (directory-files lisp-dir t "\\.el")))


(require 'init-package)        ; 配置包管理器
(require 'init-custom)         ; 默认配置
(require 'init-theme)          ; 主题配置
(require 'init-autocomplete)   ; 自动补伤全配置
(require 'init-errors)         ; 错误/警告提示引擎配置
(require 'init-project)     ; 项目管理配置
(require 'init-magit)          ; git客户端
(require 'init-dashboard)      ; 首页配置
(require 'init-ide)            ; ide配置 lsp
(require 'init-org)            ; org的相关配置
(require 'init-hydra)          ; hydra配置自定的快捷键
(require 'init-hooks)          ; 钩子和扩展功能配置文件
;;; init.el ends here
