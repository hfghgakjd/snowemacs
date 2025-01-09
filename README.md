# 我的 Emacs 配置
一个模块化的 Emacs 配置，专注于性能优化和功能组织。

## 目录结构

```
.emacs.d/
├── init.el              # 主配置文件
├── lisp/               # 模块化配置目录
│   ├── init-package.el  # 包管理配置
│   ├── init-custom.el   # 默认配置
│   ├── init-theme.el    # 主题配置
│   ├── init-org.el      # Org mode配置
│   ├── init-ide.el      # IDE/LSP配置
│   └── ...             # 其他配置模块
└── org-mode/           # org-mode相关文件
```

## 主要功能

- 包管理优化：使用 `use-package` 实现延迟加载
- LSP支持：通过 `eglot` 实现，支持 Java/C/C++/Zig
- Org-mode：完整的文档写作环境，支持 LaTeX 导出
- 项目管理：集成 `magit`
- 自动补全：优化的补全框架
- 性能优化：模块化加载，启动时间优化

## 性能特点

- 启动时间优化：使用延迟加载和按需加载
- 模块化设计：独立的功能模块，易于维护
- 包管理优化：使用镜像源和缓存机制

## 安装

1. 备份现有的 `.emacs.d` 目录（如果存在）
2. 克隆此仓库到你的主目录：
   ```bash
   git clone [repository-url] ~/.emacs.d
   ```
3. 启动 Emacs，系统会自动安装所需包

## 依赖

- Emacs 28.1 或更高版本
- 外部依赖：
  - Java LSP: Eclipse JDT Language Server
  - LaTeX: XeLaTeX
  - Fonts: Noto Sans SC, Microsoft YaHei

## 使用说明

- Org-mode 相关：
  - 文档编写：常规 org-mode 操作
  - LaTeX 导出：支持中文文档
  - Org-roam：知识管理系统

- IDE 功能：
  - Java 开发：自动 LSP 支持
  - C/C++ 开发：集成 eglot
  - 项目管理：magit 集成

## 自定义

如需自定义配置，建议：
1. 修改 `init-custom.el` 进行个性化设置
2. 新模块添加到 `lisp` 目录
3. 在 `init.el` 中引入新模块
