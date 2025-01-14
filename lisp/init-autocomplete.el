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
  :defer t                                      ; 完全延迟加载
  :commands (company-mode global-company-mode)
  :init
  (setq company-idle-delay 0.2                    ; 延迟0.2秒开始补全，平衡响应速度和性能
        company-minimum-prefix-length 2           ; 输入2个字符后触发补全，提供及时补全
        company-show-quick-access t               ; 显示快速访问键，提升选择效率
        company-tooltip-limit 10                  ; 最多显示10个候选项，避免显示性能问题
        company-tooltip-align-annotations t)      ; 对齐注释，优化显示效果
  ;; 仅在编程模式和文本模式下启用
  :hook ((prog-mode text-mode) . company-mode))

;; Ivy配置 - 通用补全框架
(use-package ivy
  :ensure t
  :defer t                                      ; 完全延迟加载
  :diminish
  :commands (ivy-mode)
  :init
  (setq ivy-count-format "(%d/%d) "              ; 显示当前/总数，便于定位
        ivy-use-virtual-buffers t)               ; 启用虚拟缓冲区，包含最近文件
  ;; 仅在需要时加载
  :hook (after-init . (lambda ()
                       (unless (bound-and-true-p ivy-mode)
                         (ivy-mode 1)))))

(provide 'init-autocomplete)
;;; init-autocomplete.el ends here
