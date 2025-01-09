;;; init-package -- package管理配置
;;; Commentary:
;;; Code:

;; 设置包管理初始化策略
(setq package-enable-at-startup nil)
(setq package-quickstart t)

;; 配置镜像源
(setq package-archives
      '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/"))
      package-archive-priorities
      '(("melpa" . 10)
        ("gnu" . 5)
        ("nongnu" . 0)))

;; 优化包初始化
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; 只在需要时刷新包内容
(unless package-archive-contents
  (package-refresh-contents))

;; 高效安装 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; 配置 use-package
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-expand-minimally t)

;; 安装基础包
(use-package better-defaults
  :defer nil)

(provide 'init-package)
;;; init-package.el ends here
