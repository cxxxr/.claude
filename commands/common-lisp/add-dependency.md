---
description: 新しい依存ライブラリを追加
argument-hint: "<library-name-or-feature>"
---

新しい依存ライブラリを調査し、追加します。

## 要求
$ARGUMENTS

## 手順

### 1. 調査フェーズ
**cl-dependency-resolver** SubAgent を使用:
- Quicklispで該当ライブラリを検索
- 複数の選択肢がある場合は比較
- ライセンス互換性を確認
- 推移的依存を確認

### 2. 評価基準
| 基準 | 確認事項 |
|------|----------|
| メンテナンス | 最終更新日、Issue対応 |
| 人気度 | 依存されている数 |
| ライセンス | プロジェクトとの互換性 |
| 依存の少なさ | 推移的依存の数 |
| ドキュメント | README、APIドキュメント |

### 3. 推奨の提示
```markdown
## 推奨: [library-name]

### 理由
- [選定理由]

### 代替候補
| ライブラリ | 特徴 | 欠点 |
|------------|------|------|
| ... | ... | ... |

### 使用例
```lisp
(ql:quickload :library-name)
(library:function ...)
```
```

### 4. 追加作業
- .asd の :depends-on に追加
- 必要なら package.lisp の :import-from を更新
- **cl-linter** で依存追加後のチェック
