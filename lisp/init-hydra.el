;;; init-hydra -- 配置自定的快捷键 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;; 安装（如果未装的话）
(use-package hydra
  :ensure t)
;; 定义hydra入口，我的常用快捷键都放在这
(defhydra hydra-main (:color blue :hint nil)
  "
^命令^
^^^^^^^^^^------------------------------
_f_: file operations        _p_: Projectile
_g_: git client             _o_: org
"
  ("f" hydra-file-dir-operations/body "文件目录操作")
  ("p" hydra-project-operations/body "Projectile项目管理")
  ("g" magit-status "git客户端")
  ("o" hydra-org-operations/body "org"))

;; 定义一个用于文件操作的 hydra
(defhydra hydra-file-dir-operations (:color blue :hint nil)
  "
^文件操作^                 ^目录操作^
^^^^^^^^^^------------------------------
_f_: 打开文件              _d_: 打开目录
_s_: 保存文件              _m_: 创建目录
_d_: 删除文件              _k_: 删除目录
_c_: 关闭文件              _j_: dired 模式
_r_: 重命名文件            _n_: 选择目录
"
  ;; 文件操作
  ("f" find-file "打开文件")
  ("s" save-buffer "保存文件")
  ("c" kill-buffer "关闭文件")
  ("x" delete-file "删除文件")
  ("r" rename-file "重命名文件")

  ;; 目录操作
  ("d" dired "打开目录")
  ("m" make-directory "创建目录")
  ("k" (lambda () (interactive) (dired-delete-file (dired-get-file-for-visit))) "删除目录")
  ("j" dired-jump "dired 模式")
  ("n" dired "选择目录")
  )
;; 绑定一个快捷键来激活这个 Hydra，Alt + 空格键
(global-set-key (kbd "M-s") 'hydra-main/body)
(provide 'init-hydra)
;;; init-hydra.el ends here
