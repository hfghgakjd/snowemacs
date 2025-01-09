;;; init-hooks.el ---                                -*- lexical-binding: t; -*-

;; 定义一个函数，用于在 mode-line（模式行）中显示当前缓冲区的字符数
(defun my-char-count-mode-line ()
  "在 mode-line 中显示当前缓冲区的字符数。"
  ;; 设置 mode-line 的格式，将字符数统计信息追加到 mode-line 中
  (setq mode-line-format
        (append mode-line-format
                '((:eval (format " [%d chars]"  ; 格式化输出字符数
                                 (- (point-max) (point-min))))))))  ; 计算字符数

;; 将 my-char-count-mode-line 函数添加到 text-mode 和 prog-mode 的钩子中
;; 这样在进入文本模式或编程模式时，mode-line 就会显示字符数
(add-hook 'text-mode-hook 'my-char-count-mode-line)
(add-hook 'prog-mode-hook 'my-char-count-mode-line)

(provide 'init-hooks)
