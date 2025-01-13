;;; init-autocomplete.el --- 自动补全配置 -*- lexical-binding: t -*-

;;; Commentary:
;;
;; 本模块提供自动补全相关的核心配置
;;
;; 设计思路:
;; - 采用懒加载机制以提升启动速度
;; - 分离代码补全(company)和命令补全(ivy)的配置
;; - 优先考虑用户体验和响应速度
;;
;; 使用场景:
;; - 程序开发时的代码补全
;; - 命令和文件查找的快速补全
;;
;; 注意事项:
;; - company-idle-delay 值会影响补全响应速度
;; - 过低的 minimum-prefix-length 可能导致过于频繁的补全
;;
;; 依赖关系:
;; - 需要预先安装 company 和 ivy 包
;; - 建议同时安装 company-statistics 提升补全精确度

;;; Code:

;; Company配置 - 代码补全框架
(use-package company
  :ensure t
  :defer 2                                       ; 延迟2秒加载
  :commands (company-mode global-company-mode)
  :hook (after-init . global-company-mode)       ; 使用hook代替init提升性能
  :init
  (setq company-idle-delay 0.2                    ; 延迟0.2秒开始补全，平衡响应速度和性能
        company-minimum-prefix-length 1           ; 输入1个字符后触发补全，提供及时补全
        company-show-quick-access t               ; 显示快速访问键，提升选择效率
        company-tooltip-limit 20                  ; 最多显示20个候选项，避免显示性能问题
        company-tooltip-align-annotations t))     ; 对齐注释，优化显示效果

;; Ivy配置 - 通用补全框架
(use-package ivy
  :ensure t
  :defer 1                                       ; 延迟1秒加载
  :diminish
  :commands (ivy-mode)
  :hook (after-init . ivy-mode)                  ; 使用hook延迟加载
  :init
  (setq ivy-count-format "(%d/%d) "              ; 显示当前/总数，便于定位
        ivy-use-virtual-buffers t)               ; 启用虚拟缓冲区，包含最近文件
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done)))                 ; 智能补全行为，优化路径补全

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
