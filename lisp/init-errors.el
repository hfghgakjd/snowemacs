;;; init-errors -- 错误/警告提示配置
;;; Commentary:
;;; Code:
(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode)
  )
(provide 'init-errors)
;;; init-errors.el ends here
