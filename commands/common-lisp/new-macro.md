---
description: 新しいマクロを作成し、検証まで行う
argument-hint: "<macro-name> <description>"
---

新しいマクロを作成し、安全性を検証します。

## 引数
$ARGUMENTS

## 手順

### 1. 設計フェーズ
Skill `cl-macro-design` を参照して:
- マクロが本当に必要か確認（関数で十分でないか）
- 適切なパラメータ設計
- gensym の使用計画

### 2. 実装フェーズ
以下の点に注意して実装:
- gensym で内部変数を保護
- once-only または let で引数の複数評価を防止
- &body は適切な位置に配置
- docstring を含める

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
