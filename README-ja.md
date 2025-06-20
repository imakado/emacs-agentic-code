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
  <b>AIとの対話をEmacsネイティブに — ストレスフリーなコーディング体験</b>
</p>

## 🎬 デモ動画

<p align="center">
  <a href="https://github.com/imakado/emacs-agentic-code/demo">
    <img src="./assets/demo-thumbnail.png" alt="Agentic Code Demo" width="80%">
  </a>
</p>

<p align="center">
  <i>クリックして動画を再生 — Emacsバッファでの快適な入力体験をご覧ください</i>
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

## なぜAgentic Codeが必要か

### 🚧 課題: ターミナルでの複雑な入力

[Claude Code](https://claude.ai/code)は革新的なAIコーディングアシスタントですが、ターミナルベースのインターフェースには大きな制約があります：

- **IME（Input Method Editor）を必要とする言語での入力困難** - 中国語、韓国語、日本語など、文字変換を必要とする言語での長文入力は非常にストレスフル
- **複数行の編集が煩雑** - ターミナルでの複数行にわたるプロンプト編集は効率的ではない
- **ファイル参照の手動管理** - プロジェクト内のファイルパスを手動で入力する必要がある
- **セッション管理の欠如** - 複数プロジェクト間の切り替えが困難

### ✨ 解決策: Emacsネイティブ統合

Agentic Codeは、これらの課題をEmacsの強力な編集機能で解決します：

- **ネイティブな入力体験** - Emacsバッファでの自然な文字入力、IMEも完全サポート
- **専用編集バッファ** - Markdownハイライト付きの快適な編集環境
- **スマートなファイル参照** - Helmとの統合で簡単にファイルを選択・送信
- **プロジェクト単位のセッション管理** - 複数プロジェクトを効率的に切り替え
- **最適化されたウィンドウレイアウト** - コンテキスト、履歴、AIアシスタントを一画面で管理

## クイックスタート

### 1分でセットアップ

```elisp
;; 1. パッケージをインストール（straight.el使用例）
(straight-use-package
 '(agentic-code :type git
                :host github
                :repo "imakado/emacs-agentic-code"))

;; 2. 設定を追加
(require 'agentic-code)
(global-set-key (kbd "C-c a") 'agentic-code-command-map)

;; 3. Claude Codeを起動
;; M-x agentic-code-transient-menu
```

### 基本的な使い方

1. **プロジェクトでAIアシスタントを起動**: `C-c a o`
2. **ファイルをAIに送信**: `C-c a f`
3. **編集バッファで快適に入力**: `C-c a e`
4. **セッション間を切り替え**: `C-c a h`


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

Magitスタイルのメニューシステムですべての機能に素早くアクセス：

- **視覚的なステータス表示** - アクティブセッション数を一目で確認
- **コンテキスト依存メニュー** - 現在の状態に応じた適切なオプション表示


## 前提条件

### 開発環境

- **Emacs**: 27.1以降（Emacs 30推奨）
- **Claude Code**: [公式サイト](https://docs.anthropic.com/en/docs/claude-code/overview)からインストール

### 依存パッケージ

```elisp
;; 必須パッケージ
(vterm "0.0.2")          ; ターミナルエミュレーター
(dash "2.19.1")          ; リスト操作ライブラリ
(s "1.13.0")             ; 文字列操作ライブラリ
(f "0.20.0")             ; ファイル操作ライブラリ
(markdown-mode "2.5")    ; Markdown編集サポート
(helm "3.9.9")           ; インクリメンタル補完インターフェース
(window-layout "1.5")    ; ウィンドウレイアウト管理
(transient "0.7.5")      ; Magitスタイルメニュー
(compat "30.1")          ; 互換性ライブラリ
(projectile "2.8.0")     ; プロジェクト管理
```

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

### straight.elを使用

```elisp
(straight-use-package
 '(agentic-code :type git
                :host github
                :repo "imakado/emacs-agentic-code"))
```


## 使い方

### 基本設定

```elisp
;; init.elに追加
(require 'agentic-code)

;; 推奨: グローバルキーバインドを設定
;; 注: 以下の説明では (kbd "C-c a") をプレフィックスキーとして設定した場合を前提としています
(global-set-key (kbd "C-c a") 'agentic-code-command-map)

;; 頻繁に使用する場合は、より押しやすい位置のキーバインドに割り当てることも推奨されます
;; 例: (global-set-key (kbd "M-SPC") 'agentic-code-command-map)

;; オプション: Claude CLIのパスを明示的に指定
(setq agentic-code-cli-command "/Users/qux/.nvm/versions/node/v20.12.2/bin/claude")

;; オプション: プロジェクトルートをヘッダーに表示
(setq agentic-code-show-project-root-in-header t)
```

### 主要な操作

#### 1. AIアシスタントの起動

```
M-x agentic-code-transient-menu  ; メインメニューを開く
C-c a o                          ; プロジェクトルートで起動
C-c a d                          ; 任意のディレクトリで起動
```

#### 2. ファイル参照の送信

```
C-c a f  ; 現在のファイルまたは選択リージョンを送信
C-c a p  ; Helmでプロジェクトファイルを選択して送信
C-c a c  ; ファイル参照をクリップボードにコピー
```

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

```
C-c a h    ; アクティブセッションを選択
C-c a w    ; ウィンドウ設定を循環
C-c a l 2  ; 2カラムレイアウト
C-c a l 3  ; 3カラムレイアウト
C-c a l r  ; レイアウトをリセット
```

### 実践的なワークフロー例

#### 複数プロジェクトでの作業

1. プロジェクトAでAIアシスタントを起動: `C-c a o`
2. 別のプロジェクトBに移動して起動: `C-c a o`
3. セッション間を切り替え: `C-c a h`
4. ウィンドウ設定を循環: `C-c a w`

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

### グローバルキーマップ

```elisp
;; 推奨設定
(global-set-key (kbd "C-c a") 'agentic-code-command-map)
```

### 主要キーバインド一覧

| キー | コマンド | 説明 |
|------|----------|------|
| `C-c a o` | `agentic-code-start-claude-code-project-root` | プロジェクトルートで起動 |
| `C-c a d` | `agentic-code-start-claude-code` | 任意のディレクトリで起動 |
| `C-c a h` | `agentic-code-helm-select-session` | セッション選択 |
| `C-c a w` | `agentic-code-cycle-window-configurations` | ウィンドウ設定を循環 |
| `C-c a f` | `agentic-code-send-claude-file-reference-dwim` | ファイル参照を送信 |
| `C-c a p` | `agentic-code-helm-projectile-send-files` | プロジェクトファイルを選択送信 |
| `C-c a c` | `agentic-code-claude-file-reference-string-to-clipboard` | 参照をコピー |
| `C-c a e` | `agentic-code-edit-in-buffer` | 編集バッファを開く |
| `C-c a m` | `agentic-code-transient-menu` | メインメニュー |
| `C-c a l 2` | `agentic-code-layout-setup-2-columns` | 2カラムレイアウト |
| `C-c a l 3` | `agentic-code-layout-setup-3-columns` | 3カラムレイアウト |
| `C-c a l r` | `agentic-code-layout-reset` | レイアウトリセット |

## カスタマイズ

### 主要なカスタマイズ変数

```elisp
;; Claude CLIコマンドのパス
(setq agentic-code-cli-command "claude")

;; 編集バッファ名のフォーマット
(setq agentic-code-edit-buffer-name-format "*agentic-code-edit [%s]*")

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

[GitHub Issues](https://github.com/imakado/emacs-agentic-code/issues)にて受け付けています。

### プルリクエスト

1. プロジェクトをフォーク
2. フィーチャーブランチを作成 (`git checkout -b feature/amazing-feature`)
3. 変更をコミット (`git commit -m 'Add amazing feature'`)
4. ブランチにプッシュ (`git push origin feature/amazing-feature`)
5. プルリクエストを作成

### コーディング規約

[CLAUDE.md](./CLAUDE.md)に記載されているEmacs Lispコーディング規約に従ってください。

## ライセンス

このプロジェクトは[GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0)の下でライセンスされています。

詳細は[LICENSE](./LICENSE)ファイルを参照してください。

## 作者

**imakado**
- GitHub: [@imakado](https://github.com/imakado)
- Email: imakado@gmail.com

## 謝辞

このプロジェクトは以下のプロジェクトに依存し、インスピレーションを受けています：

- [vterm.el](https://github.com/akermu/emacs-libvterm) - 高性能なターミナルエミュレーター
- [Claude Code CLI](https://claude.ai/code) - Anthropicの革新的なAIコーディングアシスタント
- [helm](https://github.com/emacs-helm/helm) - 強力なインクリメンタル補完フレームワーク
- [transient](https://github.com/magit/transient) - Magitスタイルのメニューシステム
- [projectile](https://github.com/bbatsov/projectile) - プロジェクト管理の決定版

Emacsコミュニティの皆様の継続的なサポートと貢献に感謝いたします。
