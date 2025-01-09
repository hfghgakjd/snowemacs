;;; package -- init-custome
;;; Commentary:
;;; Code:
(tool-bar-mode -1)
;; 隐藏菜单栏
(menu-bar-mode -1)
;; 显示行号
(global-display-line-numbers-mode 1)
;; 关闭滚动条
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; 关闭光标到边缘区域后自动翻页
(setq-default scroll-step 1 scroll-margin 0 scroll-conservatively 10000)
;; 括号自动匹配
(electric-pair-mode 1)
;; 关闭自动保存，自动备份文件
(setq auto-save-mode nil)
(setq make-backup-files nil)
;; 全屏
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
;; 字体
(cond
 ;; 如果是windows系统, 设置Consolas字体
 ((eq system-type 'windows-nt)
  (set-face-attribute 'default nil :font "Consolas-11"))
  ;; 如果是Linux系统
 ((eq system-type 'gnu/linux)
  (set-face-attribute 'default nil :font "Ubuntu Mono-12"))
 )
 ;; 设置中文字体
(dolist (charset '(kana han symbol cjk-misc bopomofo))
   (set-fontset-font (frame-parameter nil 'font) charset
                     (font-spec :family "仿宋" :size 18)))
;; 配置编码
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; 配置whitespace
(global-whitespace-mode t)
; 设置长行的阈值为 100 个字符
(setq whitespace-line-column 2000)
;; 配置yes和no的别名
(defalias 'yes-or-no-p 'y-or-n-p)
;; 选中的内容可以C-d删除
(delete-selection-mode 1)
;; 选中的内容可以粘贴覆盖
(cua-selection-mode 1)
;; 关闭启动文档
(setq inhibit-splash-screen 1)
;; 重新映射mark的快捷键
(global-set-key (kbd "C-,") 'set-mark-command)
;; package-check-signature nil
(setq package-check-signature nil)
(when (eq system-type 'windows-nt)
  (setq exec-path (append exec-path '("C:/softs/sqlite3"))))
(setenv "PATH" (concat "C:/softs/sqlite3;" (getenv "PATH")))
(setq debug-on-error t)

(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))
(setq magit-process-connection-type nil)

(provide 'init-custom)
;;; init-custom.el ends here
