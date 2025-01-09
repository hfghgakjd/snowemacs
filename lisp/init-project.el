;;; init-projectile -- 项目管理的配置
;;; Commentary:
;;; Code:

(require 'project)

;; 配置项目根目录识别
(setq project-vc-extra-root-markers '(".project" ".git" ".hg" ".svn" "Makefile"))

;; 设置项目列表文件
(setq project-list-file "~/.emacs.d/projects")

;; 手动添加项目到 project.el 的项目列表
(defun my/add-project (dir)
  "手动将 DIR 添加到 project.el 的项目列表中。"
  (interactive "D添加项目目录: ")
  (let ((projects (mapcar (lambda (p) (if (listp p) p (list p)))
                          (project-known-project-roots))))
    (unless (member (list dir) projects)
      (setq projects (append projects (list (list dir))))
      (setq project--list projects)
      ;; 保存到项目列表文件
      (with-temp-file project-list-file
        (insert (prin1-to-string projects)))
      (message "项目 %s 已添加到 project.el 列表中" dir))))

;; 手动移除项目
(defun my/remove-project (dir)
  "手动将 DIR 从 project.el 的项目列表中移除。"
  (interactive "D移除项目目录: ")
  (setq project--list (cl-remove-if (lambda (p) (string= (car p) dir)) project--list))
  (with-temp-file project-list-file
    (insert (prin1-to-string project--list)))
  (message "项目 %s 已移除" dir))

;; 禁用自动移除项目
(defun project--remove-dead-projects ()
  "Override to do nothing. Prevent auto-removal of projects.")

;; 启动时加载项目列表
(defun my/load-projects ()
  "从文件加载项目列表到 project--list。"
  (when (file-exists-p project-list-file)
    (with-temp-buffer
      (insert-file-contents project-list-file)
      (setq project--list (read (buffer-string))))))

(my/load-projects)

;; 清除不存在的项目缓存
(defun project-forget-zombie-projects ()
  "Remove non-existing projects from the known projects list."
  (interactive)
  (setq project--list
        (cl-remove-if-not #'file-directory-p project--list))
  (with-temp-file project-list-file
    (insert (prin1-to-string project--list)))
  (message "已移除不存在的项目"))

;; hydra配置
;; 定义一个用于Project项目管理的 hydra
(defhydra hydra-project-operations (:color blue :hint nil)
  "
^Project项目管理^
^^^^^^^^^^^^^^^^^^^--------------------
_p_: 切换项目        _f_: 查找文件
_d_: 查找目录        _s_: 正则查找
_g_: 全局搜索        _b_: 切换缓冲区
_r_: 忽略项目        _a_: 添加项目
_c_: 清除缓存
"
  ("p" project-switch-project "切换项目")
  ("f" project-find-file "查找文件")
  ("d" project-find-dir "查找目录")
  ("s" project-find-regexp "正则查找")
  ("g" project-find-regexp "全局搜索")
  ("b" project-switch-to-buffer "切换缓冲区")
  ("r" project-forget-project "忽略项目")
  ("a" my/add-project "添加项目")
  ("c" project-forget-zombie-projects "清除缓存"))

(provide 'init-project)
;;; init-project.el ends here
