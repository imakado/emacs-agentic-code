# emacs-agentic-code

<p align="center">
  <a href="https://github.com/imakado/emacs-agentic-code/releases">
    <img src="https://img.shields.io/badge/version-0.1.0-blue.svg" alt="Version">
  </a>
  <a href="https://www.gnu.org/licenses/gpl-3.0">
    <img src="https://img.shields.io/badge/License-GPL%20v3-blue.svg" alt="License: GPL v3">
  </a>
  <a href="https://www.gnu.org/software/emacs/">
    <img src="https://img.shields.io/badge/Emacs-27.1%2B-purple.svg" alt="Emacs 27.1+">
  </a>
</p>

<p align="center">
  <b>Vibe Coding with Claude Code in Emacs</b>
</p>

## 🎬 demo

<p align="center">
  <i>デモ動画は数日以内にアップロード予定です。</i>
</p>

## 📚 目次

- [なぜAgentic Codeが必要か](#なぜagentic-codeが必要か)
- [クイックスタート](#クイックスタート)
- [主な機能](#主な機能)
- [前提条件](#前提条件)
- [インストール](#インストール)
- [使い方](#使い方)
- [キーバインディング](#キーバインディング)
- [カスタマイズ](#カスタマイズ)
- [コントリビューション](#コントリビューション)
- [ライセンス](#ライセンス)
- [作者](#作者)
- [謝辞](#謝辞)

## なぜ Agentic Code を作ったか

### 🚧 課題: ターミナルでの複雑な入力

[Claude Code](https://claude.ai/code)は革新的なAIコーディングアシスタントですが、ターミナルベースのインターフェースには大きな制約があります：

- **IME（Input Method Editor）を必要とする言語での入力困難** - 日本語、中国語、韓国語など、文字変換を必要とする言語での長文入力は非常にストレスフル
- **複数行の編集が煩雑** - ターミナルでの複数行にわたるプロンプト編集は効率的ではない
- **複数プロジェクト間の切り替えが煩雑**

### ✨ 解決策: Emacsネイティブ統合

Agentic Codeは、これらの課題をEmacsの強力な編集機能で解決します：

- **ネイティブな入力体験** - Emacsバッファでの自然な文字入力、IMEも完全サポート
- **専用編集バッファ** - Markdownハイライト付きの快適な編集環境
- **スマートなファイル参照** - Helmとの統合で簡単にファイルを選択・送信
- **複数プロジェクトの効率的な切り替え**
- **安定したウィンドウレイアウト**

## クイックスタート

### 私の設定

```elisp
(require 'agentic-code)

;;; Customization
;; Display the project route in the terminal buffer header.
(setq agentic-code-show-project-root-in-header t)

;;; Key Binding
;; Assign the menu used with transient.el to a key binding.
;; I use “bind-key*” to ensure that this key binding takes precedence in any location.
;; Be careful, because it may overwrite key bindings in other modes.
(require 'bind-key)
(bind-key* "M-SPC" 'agentic-code-transient-menu)

;; 推奨: グローバルキーバインドを設定
(global-set-key (kbd "M-t") agentic-code-command-map)

;; My recommended key bindings for working with Claude Code in vterm.el
(keymap-set agentic-code-vterm-minor-mode-map
            "C-n"
            "<down>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-p"
            "<up>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-y"
            'vterm-yank)

;;; Window Configuration
(require 'agentic-code-window-layout-config)
(agentic-code-layout-setup-3-columns)
;; OR If you use a laptop
;; (agentic-code-layout-setup-2-columns)
```

### 基本的な使い方

**🚀 まずはこれだけ覚えればOK！**

```
M-SPC  ; Transientメニューを開く（すべての機能にアクセス）
```

初めて使う方は、`M-SPC`（Meta + スペース）を押してメニューを開くことから始めましょう。
視覚的なメニューから必要な機能を選ぶだけで、AIアシスタントを活用できます。

#### よく使う操作の例：
1. `M-SPC` → `o` : プロジェクトでAIアシスタントを起動
2. `M-SPC` → `f` : 現在のファイルをAIに送信
3. `M-SPC` → `h` : 複数のセッション間を切り替え
4. `M-SPC` → `l` → `3` : 3カラムレイアウトに変更


## 主な機能

### 🌟 ネイティブなEmacsエクスペリエンス

#### 快適な入力環境
- **Emacsバッファでの自然な編集** - 使い慣れたキーバインドとIMEサポート
- **Markdownシンタックスハイライト** - プロンプト作成時の視認性向上
- **複数行編集の自由度** - ターミナルの制約から解放

#### シームレスな統合
- **vterm.elによる完全統合** - Claude Code CLIをEmacsバッファ内で直接実行
- **プロジェクト認識** - Projectileとの連携で自動的にプロジェクトルートを検出
- **Helm統合** - 複数ファイルの一括選択 -> ファイルリファレンスの送信、複数セッションの選択 -> 表示、などの機能の実装

### 📁 インテリジェントなセッション管理

- **プロジェクト単位の独立セッション** - 各プロジェクトごとに専用のAIアシスタントセッション
- **自動復元** - 前回の作業状態を記憶し、すぐに再開可能
- **マルチプロジェクト対応** - 複数プロジェクトを同時に扱い、`C-c a h`で瞬時に切り替え
- **ウィンドウ設定の保存** - プロジェクトごとのレイアウトを自動記憶

### 📝 スマートなファイル参照システム

- **DWIM（Do What I Mean）動作** - コンテキストに応じた最適な動作
- **Helmによるファイル選択** - プロジェクト内のファイルを素早く選択
- **複数ファイルの一括送信** 

### 🪟 最適化されたウィンドウレイアウト

作業スタイルに合わせて選べる2つのレイアウト：

```
2カラムレイアウト（シンプル作業向け）:
┌─────────────┬─────────────┐
│   Context   │   Claude    │
│   (50%)     │   (50%)     │
└─────────────┴─────────────┘

3カラムレイアウト（複雑な作業向け）:
┌───────┬───────┬───────────┐
│History│Context│  Claude   │
│ (33%) │ (33%) │   (34%)   │
└───────┴───────┴───────────┘
```

- **動的レイアウト切り替え** - 作業内容に応じてレイアウトを即座に変更
- **編集バッファの自動配置** - プロンプト編集時は下部に自動表示
- **ウィンドウ設定の循環** - `C-c a w`でセッション間のレイアウトを巡回

### 🎯 直感的なTransientメニュー

Magitスタイルのメニューシステムで、すべての機能に素早くアクセスできます。

#### 📖 メニューの見方

`M-SPC`を押すと、以下のような構造のメニューが表示されます：

```
Claude Code Sessions | プロジェクト名 (+ 2 more)  ← 現在のセッション状態

Session Management        File & Editing           Layout & UI
o Start in project root   f Send file reference    l Layout management →
d Start in directory...   p Pick project files     m Open this menu
h Select session [3]      c Copy file reference    
w Cycle window configs    e Edit in buffer         q Quit
```

#### 🔤 キーの覚え方

メニューのキーは直感的に設計されています：
- **o** : Open（プロジェクトで開く）
- **f** : File（ファイルを送信）
- **h** : Helm（セッション選択）
- **p** : Pick（ファイルを選んで送信）
- **e** : Edit（編集バッファ）
- **l** : Layout（レイアウト管理）

#### ✨ 便利な機能

- **視覚的なステータス表示** - アクティブセッション数を一目で確認
- **コンテキスト依存メニュー** - 現在の状態に応じた適切なオプション表示
- **ファイル名の表示** - 操作対象のファイルが`[filename]`形式で表示
- **サブメニュー** - `l`を押すとレイアウト管理のサブメニューへ


## 前提条件

### 開発環境

- **Emacs**: 27.1以降（Emacs 30推奨）
- **Claude Code**: [公式サイト](https://docs.anthropic.com/en/docs/claude-code/overview)からインストール

## インストール
### use-package
```elisp
;; ---------- Emacs 30+  --------------
(use-package agentic-code
  :vc (:url "https://github.com/imakado/emacs-agentic-code.git"
       :rev :newest
       :main-file "agentic-code.el")
  :after (projectile magit)
  :commands (agentic-code:project-root))
```

### straight.el

```elisp
(straight-use-package
 '(agentic-code :type git
                :host github
                :repo "imakado/emacs-agentic-code"))
```


## 使い方

### 🎨 Transientメニューで始める（推奨）

初めて使う方は、`M-SPC`でTransientメニューを使うことをお勧めします。
すべての機能が視覚的に表示されるため、コマンドを覚える必要がありません。

設定については[クイックスタート](#クイックスタート)の「私の設定」を参照してください。

### 📋 主要な操作ガイド

```elisp
(require 'agentic-code)

;;; Customization
;; Display the project route in the terminal buffer header.
(setq agentic-code-show-project-root-in-header t)

;;; Key Binding
;; Assign the menu used with transient.el to a key binding.
;; I use “bind-key*” to ensure that this key binding takes precedence in any location.
;; Be careful, because it may overwrite key bindings in other modes.
(require 'bind-key)
(bind-key* "M-SPC" 'agentic-code-transient-menu)

;; 推奨: グローバルキーバインドを設定
(global-set-key (kbd "M-t") agentic-code-command-map)

;; My recommended key bindings for working with Claude Code in vterm.el
(keymap-set agentic-code-vterm-minor-mode-map
            "C-n"
            "<down>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-p"
            "<up>")
(keymap-set agentic-code-vterm-minor-mode-map
            "C-y"
            'vterm-yank)

;;; Window Configuration
(require 'agentic-code-window-layout-config)
(agentic-code-layout-setup-3-columns)
;; OR If you use a laptop
;; (agentic-code-layout-setup-2-columns)
```


#### 1. AIアシスタントの起動

**🌟 Transientメニューから（推奨）:**
```
M-SPC → o  ; プロジェクトルートで起動
M-SPC → d  ; 任意のディレクトリで起動
```

初回起動時は自動的にvtermバッファが作成され、Claude Codeが起動します。
同じプロジェクトで再度実行すると、既存のセッションに切り替わります。

#### 2. ファイル参照の送信

**🌟 Transientメニューから:**
```
M-SPC → f  ; 現在のファイルまたは選択リージョンを送信
M-SPC → p  ; Helmでプロジェクトファイルを選択して送信（複数選択可）
M-SPC → c  ; ファイル参照をクリップボードにコピー
```

💡 **ヒント**: `p`を使うと、Helmインターフェースで複数ファイルを選択できます。
`C-SPC`でマーク、`M-a`ですべて選択できます。

#### 3. 編集バッファの使用

vtermバッファ内で:
```
C-c C-e  ; 編集バッファを開く
```

編集バッファ内で:
```
C-c C-c  ; 内容を送信して閉じる
C-c C-k  ; キャンセルして閉じる
```

#### 4. セッションとレイアウト管理

**🌟 Transientメニューから:**
```
M-SPC → h  ; アクティブセッションを選択（Helm使用）
M-SPC → w  ; ウィンドウ設定を循環
M-SPC → l  ; レイアウト管理のサブメニューへ
  → 2      ; 2カラムレイアウト
  → 3      ; 3カラムレイアウト  
  → r      ; レイアウトをリセット
```

💡 **ヒント**: `w`を連続で押すと、各セッションのウィンドウ設定を巡回できます。

### 実践的なワークフロー例

#### 複数プロジェクトでの作業

1. プロジェクトAでAIアシスタントを起動: `M-SPC → o`
2. 別のプロジェクトBに移動して起動: `M-SPC → o`
3. セッション間を切り替え: `M-SPC → h`
4. ウィンドウ設定を循環: `M-SPC → w`（連続で`w`を押して切り替え）

#### 複雑なプロンプトの作成

1. vtermでClaude Codeを起動
2. `C-c C-e`で編集バッファを開く
3. 複数のファイル参照を追加:
   ```
   以下のファイルを参考に、新しい機能を実装してください：
   
   @src/main.el
   @src/utils.el#L20-50
   @tests/test-main.el
   
   要件：
   - エラーハンドリングを追加
   - ユニットテストを作成
   - ドキュメントを更新
   ```
4. `C-c C-c`で送信

## キーバインディング

### 🌟 推奨: Transientメニュー（初心者向け）

**まずはこれだけ覚えればOK！**

```elisp
;; Meta + スペースでメニューを開く
(bind-key* "M-SPC" 'agentic-code-transient-menu)
```

`M-SPC`を押すと、すべての機能にアクセスできる視覚的なメニューが表示されます。
コマンドを覚える必要がなく、直感的に操作できます。

#### Transientメニューの主要キー

| キー | 機能 | 説明 |
|------|------|------|
| `o` | プロジェクトで起動 | 現在のプロジェクトルートでClaude Codeを起動 |
| `d` | ディレクトリで起動 | 任意のディレクトリを選んで起動 |
| `f` | ファイル送信 | 現在のファイル/選択範囲を送信 |
| `p` | ファイル選択送信 | Helmで複数ファイルを選んで送信 |
| `h` | セッション選択 | アクティブなセッション一覧から選択 |
| `w` | ウィンドウ循環 | セッションのウィンドウ設定を巡回 |
| `l` | レイアウト管理 | ウィンドウレイアウトのサブメニュー |
| `e` | 編集バッファ | vterm内で長文入力用バッファを開く |
| `?` | ヘルプ | キーバインド一覧を表示 |

### 📎 オプション: コマンドマップ（上級者向け）

従来のEmacsスタイルのキーバインドを好む場合：

```elisp
;; M-tをプレフィックスキーとして使用
(global-set-key (kbd "M-t") agentic-code-command-map)
```

#### コマンドマップのキーバインド一覧

| キー | コマンド | 説明 |
|------|----------|------|
| `M-t o` | `agentic-code-start-claude-code-project-root` | プロジェクトルートで起動 |
| `M-t d` | `agentic-code-start-claude-code` | 任意のディレクトリで起動 |
| `M-t h` | `agentic-code-helm-select-session` | セッション選択 |
| `M-t w` | `agentic-code-cycle-window-configurations` | ウィンドウ設定を循環 |
| `M-t f` | `agentic-code-send-claude-file-reference-dwim` | ファイル参照を送信 |
| `M-t p` | `agentic-code-helm-projectile-send-files` | プロジェクトファイルを選択送信 |
| `M-t c` | `agentic-code-claude-file-reference-string-to-clipboard` | 参照をコピー |
| `M-t e` | `agentic-code-edit-in-buffer` | 編集バッファを開く |
| `M-t m` | `agentic-code-transient-menu` | Transientメニューを開く |
| `M-t l 2` | `agentic-code-layout-setup-2-columns` | 2カラムレイアウト |
| `M-t l 3` | `agentic-code-layout-setup-3-columns` | 3カラムレイアウト |
| `M-t l r` | `agentic-code-layout-reset` | レイアウトリセット |

## カスタマイズ

### 主要なカスタマイズ変数

```elisp
;; Claude CLIコマンドのパス
(setq agentic-code-cli-command "claude")

;; プロジェクトルートをヘッダーに表示
(setq agentic-code-show-project-root-in-header t)

;; Claude Codeの初期サジェスションを自動削除
(setq agentic-code-remove-claude-code-suggestion t)

;; ウィンドウレイアウト設定
(setq agentic-code-layout-window-height-ratio 0.3)
(setq agentic-code-layout-2col-left-size-ratio 0.5)
(setq agentic-code-layout-3col-left-size-ratio 0.33)

;; ログレベル設定（デバッグ用）
(setq agentic-code-log-level 'info) ; 'debug, 'info, 'warn, 'error, 'fatal, nil
```

### カスタムフェイス

```elisp
;; ヘッダーのプロジェクトラベル
(set-face-attribute 'agentic-code-header-project-label-face nil
                    :weight 'bold
                    :height 1.2)

;; ヘッダーのプロジェクトパス
(set-face-attribute 'agentic-code-header-project-path-face nil
                    :foreground "cyan"
                    :height 1.1)
```

## コントリビューション

### バグ報告・機能要望

[GitHub Issues](https://github.com/imakado/emacs-agentic-code/issues)

### コーディング規約

[CLAUDE.md](./CLAUDE.md)に記載されているEmacs Lispコーディング規約に従ってください。

## ライセンス

このプロジェクトは[GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0)の下でライセンスされています。

## 作者

**imakado**
- GitHub: [@imakado](https://github.com/imakado)
- X: [@imakado](https://x.com/imakado)

## 謝辞

このプロジェクトは以下のプロジェクトに、インスピレーションを受けています：

- [vterm.el](https://github.com/akermu/emacs-libvterm) - 高性能なターミナルエミュレーター
- [Claude Code](https://www.anthropic.com/claude-code) - Anthropicの革新的なAIコーディングアシスタント

Emacsコミュニティの皆様の継続的なサポートと貢献に感謝いたします。
