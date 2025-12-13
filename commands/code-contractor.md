---
description: code-contractor validateのエラーを自動修正
---

`code-contractor validate` を実行してバリデーションエラーがないか確認してください。

## 手順

1. `git diff main > /tmp/{{diff-file}}; code-contractor validate --quiet --diff /tmp/{{diff file}}` コマンドを実行
2. エラーがあれば、エラー内容を分析
3. 該当するコードを修正
4. 再度 `code-contractor validate` を実行して修正を確認
5. すべてのエラーが解消されるまで繰り返す

## 注意事項

- エラーの根本原因を理解してから修正すること
- コード品質を維持しながら修正を行う
- 修正が他の部分に影響を与えないか確認する
