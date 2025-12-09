# Claude Code 個人設定

このディレクトリには Claude Code の個人用設定を定義する。

## ディレクトリ構造

```
~/.claude/
├── README.md        # このファイル
├── CLAUDE.md        # グローバル設定（任意）
├── settings.json    # Claude Code設定
└── commands/        # カスタムスラッシュコマンド
```

---

## カスタムスラッシュコマンド

`commands/` ディレクトリにはカスタムスラッシュコマンドを定義する。

### コマンド一覧

| コマンド | 説明 |
|----------|------|
| `/analyze:architecture` | リポジトリからARCHITECTURE.mdを生成 |
| `/analyze:code` | ディレクトリのアーキテクチャと設計を解説 |
| `/claude:create-command` | 新しいカスタムスラッシュコマンドを作成 |
| `/git:commit` | 変更内容を分析しコミット作成 |
| `/git:create-pr` | プルリクエストを作成 |
| `/git:rename-branch` | ブランチ名を変更 |
| `/refactor:dead-code` | 未使用コードを検出・削除 |
| `/refactor:extract-common` | 重複コードを共通コンポーネントとして抽出 |

### ディレクトリ構造

```
commands/
├── analyze/             # コード分析・解説
│   ├── architecture.md
│   └── code.md
├── claude/              # Claude Code関連
│   └── create-command.md
├── git/                 # Git操作
│   ├── commit.md
│   ├── create-pr.md
│   └── rename-branch.md
└── refactor/            # リファクタリング
    ├── dead-code.md
    └── extract-common.md
```

### コマンドファイルの書式

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
