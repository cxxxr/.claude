# Claude Code 個人設定

Common Lisp開発に特化したClaude Code拡張と、汎用的な開発支援ツールを含む個人設定。

## ディレクトリ構造

```
~/.claude/
├── README.md           # このファイル
├── CLAUDE.md           # グローバル設定
├── settings.json       # Claude Code設定
├── commands/           # カスタムスラッシュコマンド
├── agents/             # 特化型エージェント
└── skills/             # 知識ベース・ガイドライン
```

---

## Commands（スラッシュコマンド）

### Claude Code関連

| コマンド | 説明 |
|----------|------|
| `/claude:create-command` | 新しいカスタムコマンドを対話的に作成 |
| `/claude:refactor-claude-md` | CLAUDE.mdをベストプラクティスに基づきリファクタリング |

### コード分析

| コマンド | 説明 |
|----------|------|
| `/analyze:code` | ディレクトリのアーキテクチャと設計を解説 |
| `/analyze:architecture` | ファクトベースのARCHITECTURE.mdを自動生成 |

### リファクタリング

| コマンド | 説明 |
|----------|------|
| `/refactor:dead-code` | 8項目のフィルターで未使用コードを安全に検出・削除 |
| `/refactor:extract-common` | 10項目の偶発的同型フィルターで重複コードを評価・抽出 |

### Nix

| コマンド | 説明 |
|----------|------|
| `/nix:init-flake` | Nix Flakeを導入（CL向け高度設定対応） |

### Git

| コマンド | 説明 |
|----------|------|
| `/git:commit` | 変更内容を分析しコミット作成 |
| `/git:create-pr` | プルリクエストを作成 |
| `/git:rename-branch` | ブランチ名を変更 |

### Common Lisp

| コマンド | 説明 |
|----------|------|
| `/common-lisp:organize-import` | defpackageの:use/:import-from/:exportを整理・ソート |
| `/common-lisp:init-project` | 新規CLプロジェクトを初期化 |
| `/common-lisp:add-dependency` | 依存関係を追加 |
| `/common-lisp:new-macro` | 新しいマクロを作成 |
| `/common-lisp:review` | CLコードレビュー |
| `/common-lisp:pr-check` | PR前のチェック |
| `/common-lisp:mcp-eval` | MCP経由でLisp式を評価 |

### その他

| コマンド | 説明 |
|----------|------|
| `/code-contractor` | code-contractor validateのエラーを自動修正 |

---

## Agents（特化型エージェント）

| エージェント | ユースケース |
|--------------|--------------|
| `project-manager` | タスク分解・優先順位付け、進捗管理、リスク分析、WBS作成 |
| `cl-codebase-analyst` | CLプロジェクトのパッケージ依存関係・デッドコード・API分析 |
| `cl-macro-verifier` | マクロ展開検証、変数捕捉・複数評価問題の検出 |
| `cl-test-runner` | Roveテスト実行と失敗原因分析 |
| `cl-dependency-resolver` | 依存関係の解決 |
| `cl-linter` | コード品質チェック |

---

## Skills（知識ベース）

| スキル | 内容 |
|--------|------|
| `cl-coding-style` | 命名規則、インデント、パッケージ参照、when/unless使い分け |
| `cl-macro-design` | 衛生的マクロ、gensym、once-only、構造パターン |
| `cl-condition-system` | condition/restart分離、handler-bind vs handler-case |
| `cl-asdf-system` | defsystem定義、ディレクトリ構成、テスト統合 |
| `cl-clos-patterns` | CLOSのOOPパターン |
| `cl-mallet-linter` | malletリンターの設定・ルール・自動修正 |

---

## ユースケース

### Common Lispプロジェクト開発

```bash
/nix:init-flake common-lisp       # 開発環境構築
/common-lisp:init-project         # プロジェクト初期化
/common-lisp:organize-import      # パッケージ整理
```

### コードベース分析・リファクタリング

```bash
/analyze:architecture             # ARCHITECTURE.md自動生成
/refactor:dead-code src/          # デッドコード検出・削除
/refactor:extract-common src/     # 重複コード抽出
```

### マクロ開発

- `cl-macro-verifier` エージェントで展開結果を検証
- `cl-macro-design` スキルで設計ガイドラインを適用

### プロジェクト管理

- `project-manager` エージェントでタスク分解・リスク分析

---

## コマンド作成ガイド

### ファイル形式

```markdown
---
description: コマンドの簡潔な説明（必須）
argument-hint: "<引数の説明>"  # オプション
allowed-tools: ["bash", "read"]  # オプション
---

コマンド実行時の指示文
```

### 利用可能な変数

| 変数 | 説明 |
|------|------|
| `$ARGUMENTS` | すべての引数 |
| `$1`, `$2`, ... | 個別の引数 |

### 特殊構文

| 構文 | 説明 |
|------|------|
| `!command` | シェルコマンドを実行 |
| `@path` | ファイル内容を埋め込み |

### 命名規則

- ファイル名はケバブケース（例: `dead-code.md`）
- 関連するコマンドはサブディレクトリでグループ化
- コマンド名は動詞または動詞句で始める

---

## 設計思想

1. **安全性重視**: 削除・抽出は複数の評価項目で検証し誤操作を防止
2. **Common Lisp特化**: condition system、CLOS、マクロ等の言語固有機能をサポート
3. **自動化**: mallet連携で品質チェックを自動化
4. **再現性**: Nix Flakeで開発環境を宣言的に管理
