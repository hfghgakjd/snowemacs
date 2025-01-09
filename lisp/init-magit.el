;;; init-magit -- git客户端magit配置
;;; Commentary:
;;; Code:

(use-package magit
  :defer t
  :init
  (setq magit-refresh-status-buffer nil) ;减少自动刷新
  :config
  (setq magit-auto-revert-mode-lighter "")
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-diff-refine-hunk t)
  ;; 优化性能设置
  (setq magit-refresh-verbose nil)
  (setq magit-status-buffer-switch-function 'switch-to-buffer)
  (setq magit-process-popup-time 10))

(provide 'init-magit)
;;; init-magit.el ends here
