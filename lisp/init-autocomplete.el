;;; init-autocomplete.el -- 自动补全配置
;;; Commentary:
;;; 配置自动补全框架，包括 company 用于代码补全和 ivy 用于命令补全
;;; Code:

;; Company配置 - 用于代码和文本的自动补全
(use-package company
  :ensure t
  :defer 1                    ;; 延迟1秒加载，优化启动速度
  :hook (after-init . global-company-mode)  ;; 在初始化后启用全局补全
  :custom
  (company-idle-delay 0.2)    ;; 延迟0.2秒后弹出补全菜单
  (company-minimum-prefix-length 1)  ;; 输入1个字符后开始补全
  (company-show-quick-access t)      ;; 显示快捷键提示
  (company-tooltip-limit 20)         ;; 补全菜单中显示20个候选项
  (company-tooltip-align-annotations t))  ;; 对齐注释说明

;; Ivy配置 - 用于增强型命令补全和选择框架
(use-package ivy
  :ensure t
  :defer 0.1                  ;; 轻微延迟加载以优化启动
  :diminish                   ;; 在模式行中隐藏ivy模式
  :bind (:map ivy-minibuffer-map
         ("RET" . ivy-alt-done))  ;; 回车键使用alt-done行为，更智能的补全
  :custom
  (ivy-count-format "(%d/%d) ")    ;; 显示当前选项/总选项数
  (ivy-use-virtual-buffers t)      ;; 在buffer列表中包含最近打开的文件
  :config
  (ivy-mode 1))                    ;; 启用ivy模式

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
