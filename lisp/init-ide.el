;;; init-ide.el --- Eglot和DAP模式的配置
;;; Commentary:
;;; Code:

;; 延迟加载eglot
(use-package eglot
  :ensure t
  :defer t
  :init
  ;; 定义Java LSP启动函数，仅在需要时执行
  (defun my/setup-java-lsp ()
    (add-to-list 'eglot-server-programs
                 `(java-mode . ("C:/softs/jdk/bin/java"
                               "-Declipse.application=org.eclipse.jdt.ls.core.id1"
                               "-Dosgi.bundles.defaultStartLevel=4"
                               "-Declipse.product=org.eclipse.jdt.ls.core.product"
                               "-noverify"
                               "-Xmx2G"
                               "-XX:+UseG1GC"
                               "-XX:+UseStringDeduplication"
                               "-jar"
                               "C:/softs/jdtls/plugins/org.eclipse.equinox.launcher_1.6.900.v20240613-2009.jar"
                               "-configuration"
                               "C:/softs/jdtls/config_win"
                               "-data"
                               "C:/workspace/java"))))

  ;; 仅在打开相应文件类型时启动eglot
  :hook
  ((java-mode . (lambda ()
                  (my/setup-java-lsp)
                  (eglot-ensure)))
   (c-mode . eglot-ensure)
   (c++-mode . eglot-ensure)
   (zig-mode . eglot-ensure)))

(provide 'init-ide)
;;; init-ide.el ends here
