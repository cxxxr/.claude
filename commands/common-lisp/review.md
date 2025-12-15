---
description: Common Lispコードの包括的レビュー
argument-hint: "[path]"
---

指定されたパス（デフォルト: src/）のCommon Lispコードをレビューします。

## 対象パス
$ARGUMENTS

（引数がない場合は `src/` を対象とする）

## SubAgentの使用

### 1. cl-linter を使用
- malletで品質チェック
- 自動修正可能な問題は `--fix-dry-run` で確認

### 2. cl-codebase-analyst を使用
- コード構造の分析
- 未使用コードの検出
- 複雑度の高い関数の特定

### 3. Skill cl-coding-style を参照
- 命名規則の確認
- フォーマットの確認

## レポート形式

```markdown
# コードレビュー: [path]

## 品質スコア
X / 100

## 検出された問題

### 重大度: 高
[即座に修正すべき問題]

### 重大度: 中
[修正を推奨する問題]

### 重大度: 低
[スタイルの提案]

## 良い点
[コードの良い部分を褒める]

## 改善提案
[具体的な改善案]
```
