# Git コミット

変更内容を分析し、適切なコミットメッセージでコミットを作成してください。

## 手順

1. 現在の状態を確認：
   - `git status` で変更されたファイルを確認
   - `git diff --staged` でステージングされた変更を確認
   - `git diff` でステージングされていない変更を確認
   - `git log --oneline -5` で最近のコミットメッセージのスタイルを確認

2. 変更内容を分析：
   - 変更の種類（新機能、バグ修正、リファクタリング等）を特定
   - 変更の目的と影響範囲を把握

3. 必要に応じてファイルをステージング：
   - 関連する変更をまとめてステージング
   - 無関係な変更は別コミットに分ける

## コミットメッセージの形式

Conventional Commits形式に従ってください：

```
<type>: <description>

[optional body]
```

### Type（種類）

- `feat` - 新機能の追加
- `fix` - バグ修正
- `refactor` - リファクタリング（機能変更なし）
- `docs` - ドキュメントのみの変更
- `style` - コードの意味に影響しない変更（フォーマット等）
- `test` - テストの追加・修正
- `chore` - ビルドプロセスや補助ツールの変更

### Description（説明）

- 英語で簡潔に記述
- 現在形で書く（"added" ではなく "add"）
- 先頭は小文字
- 末尾にピリオドをつけない

### 例

```
feat: add user authentication system
fix: resolve login validation error
refactor: simplify database connection logic
docs: update API documentation
```

## 実行

1. すべての変更を確認し、ステージングする
2. 適切なコミットメッセージを決定
3. コミットを作成（`git commit -m "..."`）
4. コミット結果をユーザーに報告

## 注意事項

- 機密情報（.env、credentials等）をコミットしない
- 大きな変更は論理的な単位で分割することを提案
- リモートへのpushは自動で行わない
- コミットメッセージに署名やフッター（Generated with Claude Code、Co-Authored-By等）を追加しない
