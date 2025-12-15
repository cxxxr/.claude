---
description: Common Lisp PR作成前の品質チェック（lint + test + 依存確認）
---

PR作成前の品質チェックを実行します。以下のSubAgentを**並列で**使用してください：

## 並列実行するSubAgents

1. **cl-linter** - malletによるコード品質チェック
   - `mallet src/` を実行
   - 警告があれば修正を提案

2. **cl-test-runner** - Roveテストの実行
   - 全テストを実行
   - 失敗があれば原因を分析

3. **cl-codebase-analyst** - 変更の影響分析
   - 変更されたファイルの依存関係を確認
   - 未使用エクスポートがないか確認

## 結果のまとめ

各SubAgentの結果を統合し、以下の形式で報告：

```markdown
# PR品質チェック結果

## Lint結果
[cl-linter の結果]

## テスト結果
[cl-test-runner の結果]

## 影響分析
[cl-codebase-analyst の結果]

## 総合判定
- [ ] Lint: OK/NG
- [ ] Tests: OK/NG
- [ ] 依存関係: OK/NG

## 推奨アクション
[問題があれば修正方法を提案]
```
