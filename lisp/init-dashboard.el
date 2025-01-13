;;; init-dashboard.el --- 启动页面配置 -*- lexical-binding: t no-byte-compile: t -*-

;;; Commentary:
;;
;; 本模块提供Emacs启动页面的个性化配置
;;
;; 设计思路:
;; - 提供友好的欢迎界面
;; - 快速访问最近的文件和项目
;; - 个性化定制显示内容
;;
;; 使用场景:
;; - Emacs启动时的首页显示
;; - 快速访问常用资源
;;
;; 注意事项:
;; - 需要图标支持请安装all-the-icons
;; - 项目管理依赖project.el
;;
;; 依赖关系:
;; - 需要预先安装dashboard包
;; - 可选依赖all-the-icons提供图标支持

;;; Code:

(use-package dashboard
  :ensure t
  :defer t                                       ; 延迟加载
  :hook (after-init . dashboard-setup-startup-hook) ; 使用hook延迟初始化
  :init
  (setq dashboard-items '((recents  . 5)         ; 减少显示项数量提升加载速度
                         (bookmarks . 5)
                         (projects . 5))
        dashboard-display-icons-p nil)            ; 默认禁用图标加快加载
  :config
  (setq dashboard-banner-logo-title "欢迎您！邵静！"    ; 设置个性化欢迎语
        dashboard-projects-backend 'project-el          ; 使用内置project管理器
        dashboard-startup-banner 'official             ; 使用官方Logo
        dashboard-set-file-icons t))                    ; 启用文件图标显示

(provide 'init-dashboard)
;;; init-dashboard.el ends here
