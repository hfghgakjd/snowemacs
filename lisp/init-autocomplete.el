;;; package -- init-autocomplete
;;; Commentary:
;;; Code:
;; 文本提示的配置 - company
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay .2                ; 延时多少秒后弹出
        company-minimum-prefix-length 1      ; 至少几个字符后开始补全
        company-show-quick-access t          ; 选项快速访问
        company-tooltip-limit 20             ; 菜单里的可选数量
        company-tooltip-align-annotations t  ; 注释贴右侧对齐
        ))
;; 命令提示的配置，M-x后命令的提示 - ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-format-function #'ivy-format-function-arrow)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  )
(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
