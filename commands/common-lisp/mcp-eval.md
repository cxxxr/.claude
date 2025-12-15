---
description: cl-mcpのrepl-evalツールでLispコードを評価
argument-hint: "<lisp-expression>"
---

cl-mcp の `repl-eval` ツールを使用して Lisp コードを評価します。

## 評価する式
$ARGUMENTS

## 使用方法

cl-mcp MCP サーバーが利用可能な場合、`repl-eval` ツールを使用してください。

### repl-eval パラメータ
- `code`: 評価するLisp式
- `package`: パッケージコンテキスト（デフォルト: "CL-USER"）
- `timeout`: タイムアウト秒数（デフォルト: 30）

### 例

```json
{
  "tool": "repl-eval",
  "arguments": {
    "code": "(+ 1 2 3)",
    "package": "CL-USER"
  }
}
```

## 注意事項

1. **パッケージ指定**: 必ず適切なパッケージを指定
2. **副作用**: 評価結果はREPLセッションに反映される
3. **永続化**: 変更を保存するには `lisp-edit-form` を使用

## MCPサーバーが利用できない場合

nix-shell経由で直接評価:

```bash
nix-shell -p sbcl --run "sbcl --eval '(load \"~/.quicklisp/setup.lisp\")' --eval '$ARGUMENTS'"
```
