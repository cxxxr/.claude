# カスタムスラッシュコマンド

このディレクトリには個人用のカスタムスラッシュコマンドを定義する。

## ディレクトリ構造

```
commands/
├── CLAUDE.md          # このファイル
├── git/               # Git関連コマンド
│   ├── commit.md      # /git:commit
│   ├── create-pr.md   # /git:create-pr
│   └── rename-branch.md # /git:rename-branch
└── refactor/          # リファクタリング関連コマンド
    └── dead-code.md   # /refactor:dead-code
```

## コマンドファイルの書式

```markdown
---
description: コマンドの簡潔な説明（必須）
argument-hint: "<引数の説明>"  # オプション
allowed-tools: ["bash", "read"]  # オプション
---

コマンド実行時の指示文
```

## 利用可能な変数

| 変数 | 説明 |
|------|------|
| `$ARGUMENTS` | すべての引数 |
| `$1`, `$2`, ... | 個別の引数 |

## 特殊構文

| 構文 | 説明 |
|------|------|
| `!command` | シェルコマンドを実行 |
| `@path` | ファイル内容を埋め込み |

## 命名規則

- ファイル名はケバブケース（例: `dead-code.md`）
- 関連するコマンドはサブディレクトリでグループ化
- コマンド名は動詞または動詞句で始める
