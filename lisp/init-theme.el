;;; init-theme -- 主题配置
;;; Commentary:
;;; Code:
(use-package doom-themes
  :ensure t
  :config
  ;; 全局配置
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  ;; 加载主题
  (load-theme 'doom-acario-light t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; use "doom-colors" for less minimal icon theme
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )
;; 自定义的背景，绿豆沙色
;; 自定义的注释颜色，紫色
(custom-set-faces
 '(default ((t (:background "#c7eacc"))))
 '(font-lock-comment-face ((t (:foreground "#9c00db"))))
 '(whitespace-space ((t (:foreground "#afafaf"))))
 '(whitespace-tab ((t (:foreground "#afafaf"))))
 '(whitespace-newline ((t (:foreground "#afafaf"))))
 )
;;; all-the-icons配置
(use-package all-the-icons
  :ensure t
  )

(provide 'init-theme)
;;; init-theme.el ends here
