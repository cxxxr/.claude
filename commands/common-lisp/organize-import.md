---
description: defpackageの:use/:import-from/:exportを整理
argument-hint: "<file-path>"
---

指定されたCommon Lispファイルの `defpackage` フォームを整理してください。

## 対象ファイル

$ARGUMENTS

## 整理ルール

### 1. :use 節
- アルファベット順にソート
- 重複を削除
- 使用していないパッケージがあれば警告

### 2. :import-from 節
- パッケージ名でアルファベット順にソート
- 各パッケージ内のシンボルもアルファベット順にソート
- 重複シンボルを削除
- 使用していないシンボルがあれば警告

### 3. :export 節
- アルファベット順にソート
- 重複を削除
- 定義されていないシンボルがあれば警告

### 4. :shadow / :shadowing-import-from 節
- 存在する場合はアルファベット順にソート

## フォーマット規則

```lisp
(defpackage #:package-name
  (:use #:cl
        #:other-package)
  (:import-from #:alexandria
                #:if-let
                #:when-let)
  (:import-from #:another-pkg
                #:some-symbol)
  (:export #:exported-func-1
           #:exported-func-2))
```

- 各節は `:use` → `:import-from` → `:export` → その他 の順序
- シンボルは `#:` プレフィックス形式で統一（または既存スタイルを維持）
- 1行に1シンボル、適切なインデント

## 出力

- 整理後の `defpackage` フォームを提示
- 変更点のサマリーを報告
- 警告がある場合は一覧表示

## 注意事項

- 既存のコメントは保持
- ファイル内に複数の `defpackage` がある場合はすべて処理
- 破壊的変更の前に確認を求める
