# 在 VS Code 开发 Borrowed Light

打开整个 `CCP` 文件夹，而不是只打开一个 `.hs` 文件。
如果安装扩展时 VS Code 已经打开，先按 `Ctrl+Shift+P`，执行
`Developer: Reload Window`（重新加载窗口）。

这台 Windows 电脑的工具链在 `C:\ghcup\bin`：GHC 9.6.7、Cabal 3.12.1.0、
Haskell Language Server 2.14.0.0。已安装 `haskell.haskell` 扩展。
项目的 `cabal.project` 固定使用 GHC 9.6.7，并包含修改过的 Gloss 源码。
`hie.yaml` 明确标出逻辑库、游戏、测试和两个本地 Gloss 包对应的 Cabal 组件，
供语言服务器加载。

## 常用操作

| 操作 | 方法 |
|---|---|
| 编译 | `Ctrl+Shift+B`，默认任务为 `Haskell: Build` |
| 运行游戏 | `Ctrl+Shift+P` → `Tasks: Run Task` → `Haskell: Run game` |
| 全屏运行 | 同上，选择 `Haskell: Run game (fullscreen)`；`Alt+F4` 关闭游戏 |
| 测试 | 同上，选择 `Haskell: Test` |
| 交互执行游戏逻辑 | 同上，选择 `Haskell: REPL (game logic)`；输入 `:quit` 退出 |
| 查看类型与文档 | 鼠标悬停在函数或变量上 |
| 跳转定义 | `F12` |
| 重命名符号 | `F2` |
| 手动格式化 | `Shift+Alt+F`；没有启用保存时自动格式化 |

第一次打开 Haskell 文件时，语言服务器需要加载项目依赖。可在
“输出 / Output”面板的 Haskell 通道查看进度。若没有补全，运行命令
`Haskell: Restart Haskell LSP Server`，并确认状态栏没有限制扩展运行的工作区提示。

编译前先关闭旧游戏窗口，Windows 不能覆盖正在运行的 `.exe`。
运行任务会先编译，再从项目根目录启动，因此开场动画等相对路径素材能够找到。
终端也可以直接执行 `cabal build all`、`cabal run borrowed-light` 和
`cabal test spec --test-show-details=direct`。

## 代码位置

- `src/Game/`：游戏规则、关卡、存档选择流程等纯逻辑。
- `app/Main.hs`：窗口、输入、时间循环和存档读写入口。
- `app/Figures.hs`：游戏中的人物和物件绘制。
- `app/Render.hs`、`app/Hud.hs`：关卡画面与界面。
- `test/Main.hs`：规则测试。

工作区的 `.vscode/settings.json` 使用本机的 GHCup 路径。换电脑时修改
`haskell.serverExecutablePath` 与终端 PATH；并将工具链目录加入用户 PATH。
任务中的 Windows PATH 在 `.vscode/tasks.json`。其他操作系统可移除这些 Windows 路径设置。

本配置提供编译、运行和语言服务，没有配置 F5 图形断点调试器。

参考：[Haskell 扩展官方说明](https://github.com/haskell/vscode-haskell)。
