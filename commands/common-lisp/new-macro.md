---
description: 新しいマクロを作成し、検証まで行う
argument-hint: "<macro-name> <description>"
---

新しいマクロを作成し、安全性を検証します。

## 引数
$ARGUMENTS

## 手順

### 1. 設計フェーズ (Google Style Guide準拠)
Skill `cl-macro-design` を参照して:
- マクロが本当に必要か確認（関数+inline で十分でないか）
- CALL-WITH スタイルを検討（マクロは薄く、ロジックは補助関数へ）
- 適切なパラメータ設計（`-form` サフィックス、拡張用の空パラメータリスト）
- gensym / alexandria:once-only の使用計画

### 2. 実装フェーズ
以下の点に注意して実装:
- gensym で内部変数を保護
- alexandria:once-only で引数の複数評価を防止
- &body は適切な位置に配置（拡張可能な `(() &body body)` 形式を推奨）
- 評価される引数には `-form` サフィックスを付ける
- docstring を含める
- バッククォート内に複雑なロジックを書かない

### 3. 検証フェーズ
**cl-macro-verifier** SubAgent を使用:
- macroexpand-1 で展開結果を確認
- 副作用のある引数でテスト
- ネストした使用でテスト

### 4. テスト作成
Rove でテストケースを作成:
```lisp
(deftest test-<macro-name>
  ;; 基本動作
  (ok ...)
  ;; エッジケース
  (ok ...)
  ;; 副作用のある引数
  (let ((n 0))
    (macro-name (incf n))
    (ok (= n 1))))
```

## 出力形式

```markdown
# マクロ: <macro-name>

## 定義
```lisp
(defmacro ...)
```

## 展開例
入力: `(macro-name ...)`
展開: `(let ...)`

## 検証結果
- 変数捕捉: OK
- 複数評価: OK
- 評価順序: OK

## テストケース
```lisp
(deftest ...)
```
```
