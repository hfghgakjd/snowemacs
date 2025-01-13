;;; init-errors.el --- 语法检查与错误提示配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 本模块提供实时语法检查和错误提示功能
;;
;; 设计思路:
;; - 使用 flycheck 提供实时语法检查
;; - 继承 Emacs 自身的加载路径确保正确性
;; - 全局启用以覆盖所有编程模式
;;
;; 使用场景:
;; - 代码编写过程中的实时语法检查
;; - 编程规范compliance检查
;;
;; 注意事项:
;; - 需要相应语言的检查器支持
;; - 可能会影响编辑器性能
;;
;; 依赖关系:
;; - 需要预先安装 flycheck 包
;; - 特定语言需要安装对应checker

;;; Code:

(use-package flycheck
  :ensure t
  :defer t                                       ; 完全延迟加载
  :diminish                                      ; 从模式行隐藏，减少显示开销
  :hook (prog-mode . flycheck-mode)              ; 仅在编程模式下启用，避免全局开销
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save idle-change mode-enabled) ; 仅在保存、空闲和启用时检查
        flycheck-idle-change-delay 2.0           ; 空闲2秒后才检查，减少CPU使用
        flycheck-idle-buffer-switch-delay 3.0    ; 切换buffer后等待3秒再检查
        flycheck-display-errors-delay 0.9)       ; 显示错误延迟，避免频繁刷新
  :config
  (setq-default flycheck-disabled-checkers       ; 禁用不需要的检查器
                '(emacs-lisp-checkdoc)))         ; 例如禁用文档格式检查

(provide 'init-errors)
;;; init-errors.el ends here
