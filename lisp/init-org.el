;;; init-org.el -- Org mode 配置
;;; Commentary:
;;; 配置 Org mode 以适应 LaTeX 导出、Org Babel 语言支持以及其他功能。
;;; Code:

;; 延迟加载主要的org配置
(with-eval-after-load 'org
  ;; 基础设置
  (setq org-confirm-babel-evaluate nil)
  
  ;; LaTeX导出相关设置
  (with-eval-after-load 'ox-latex
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"
            "xelatex -8bit -shell-escape -interaction nonstopmode -output-directory %o %f"))

    (setq org-latex-hyperref-template nil
          org-latex-with-hyperref t
          org-latex-packages-alist '(("colorlinks=true,linkcolor=blue,urlcolor=cyan" "hyperref" nil)))

    ;; LaTeX类设置
    (setq org-latex-classes nil)
    (add-to-list 'org-latex-classes
                 '("cn-article"
                   "\\documentclass[10pt,a4paper]{article}
\\usepackage{fontspec} % 引入 fontspec 包
\\usepackage{xeCJK} % 引入 xeCJK 包处理中文
\\usepackage{xcolor} % 引入xcolor
\\definecolor{hltextcolor}{RGB}{255, 69, 0} % 配置文本单词高亮颜色
\\definecolor{bgcolor}{RGB}{245,245,245} % 定义代码块背景颜色为浅灰色
% 设置英文字体
\\setmainfont{Noto Sans SC}
\\setsansfont{Arial}
\\setmonofont{Noto Sans SC} % 设置等宽字体，用于代码
% 设置中文字体
\\setCJKmainfont{Microsoft YaHei} % 设置中文主字体为微软雅黑
\\setCJKsansfont{Microsoft YaHei UI} % 设置中文无衬线字体为微软雅黑UI
\\setCJKmonofont{Microsoft YaHei Mono} % 设置中文等宽字体为微软雅黑Mono
% 设置段落和行距
\\setlength{\parindent}{2em} % 首行缩进
\\setlength{\parskip}{0.5em} % 段落间距
\\linespread{1.5} % 行间距
\\usepackage[cache=false]{minted}
\\setminted{fontsize=\\footnotesize, breaklines, breakanywhere, bgcolor=bgcolor, frame=lines, framesep=2mm}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    ;; LaTeX导出钩子
    (add-to-list 'org-export-filter-final-output-functions 'org-latex-highlight-texttt)
    (add-hook 'org-export-before-processing-hook 'my/org-latex-remove-hypersetup)))

;; 延迟加载org-babel语言支持
(with-eval-after-load 'ob
  (defun my/setup-org-babel-languages ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)  ;; 保留最常用的语言
       (shell . t)
       (python . t)
       ;; 按需加载其他语言
       (latex . t))))
  (add-hook 'org-mode-hook #'my/setup-org-babel-languages))

;; org-roam延迟加载
(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory "C:/workspace/documents")
  (org-roam-db-location "C:/workspace/documents/org-roam.db")
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol))

;; 基本的org-mode钩子
(add-hook 'org-mode-hook
          (lambda ()
            (setq truncate-lines nil)
            ;; 仅在需要时加载reftex
            (when (member "REFTEX" org-todo-keywords-1)
              (reftex-mode t))))

;; 自动插入模板 - 延迟加载
(with-eval-after-load 'autoinsert
  (define-auto-insert '(org-mode . "Chinese Org skeleton")
    '("Description: "
      "#+LATEX_CLASS: cn-article\n"
      "#+LATEX_CLASS_OPTIONS: [oneside,A4paper,12pt]\n")))

;; 打印设置 - 延迟加载
(with-eval-after-load 'ps-print
  (setq ps-paper-type 'a4
        ps-font-size 16.0
        ps-print-header nil
        ps-landscape-mode nil))

;; 保持hydra配置但延迟加载
(with-eval-after-load 'hydra
  (defhydra hydra-org-operations (:color blue :hint nil)
    "
^节点^
^^^^^^^^^-----------
_f_: 查找节点
"
    ("f" org-roam-node-find "查找节点")))

;; 解除快捷键绑定
(add-hook 'org-mode-hook (lambda ()
                          (local-unset-key (kbd "C-,"))))

(provide 'init-org)
;;; init-org.el ends here
