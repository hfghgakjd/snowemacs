;;; init-dashboard -- 首页配置 -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;;; 配置开始

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-banner-logo-title "欢迎您！邵静！") ;; 个性签名，随读者喜好设置
  (setq dashboard-projects-backend 'project-el) ;; 使用 project.el 作为项目管理工具
  (setq dashboard-startup-banner 'official) ;; 也可以自定义图片
  (setq dashboard-items '((recents  . 5)   ;; 显示多少个最近文件
                          (bookmarks . 5)  ;; 显示多少个最近书签
                          (projects . 10))) ;; 显示多少个最近项目
  (setq dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))
(provide 'init-dashboard)
;;; init-dashboard.el ends here
