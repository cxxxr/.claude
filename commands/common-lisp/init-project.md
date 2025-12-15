---
description: Common Lispプロジェクトを初期化（flake.nix + cl-mcp + mallet）
argument-hint: "<project-name>"
---

Common Lispプロジェクトを初期化します。Nix flake、cl-mcp統合、mallet設定を含みます。

## プロジェクト名
$ARGUMENTS

## 作成するファイル

```
<project-name>/
├── flake.nix              # Nix開発環境
├── .envrc                 # direnv設定
├── <project-name>.asd     # ASDFシステム定義
├── src/
│   ├── package.lisp       # パッケージ定義
│   └── main.lisp          # メインコード
├── tests/
│   ├── package.lisp       # テスト用パッケージ
│   └── main.lisp          # テストコード
├── CLAUDE.md              # Claude Code設定
├── .mallet.lisp           # リンター設定
├── README.md
├── LICENSE
└── .gitignore
```

## 各ファイルの内容

### 1. flake.nix

```nix
{
  description = "<project-name> - Common Lisp project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    cl-mcp = {
      url = "github:cl-ai-project/cl-mcp";
      flake = false;
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      perSystem = { pkgs, ... }: {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [ sbcl rlwrap curl ];

          shellHook = ''
            export QUICKLISP_HOME="$HOME/.quicklisp"
            export CL_SOURCE_REGISTRY="${inputs.cl-mcp}/:$PWD//:$QUICKLISP_HOME/local-projects//"
            export MCP_PROJECT_ROOT="$PWD"

            if [ ! -f "$QUICKLISP_HOME/setup.lisp" ]; then
              echo "Installing Quicklisp..."
              INSTALLER=$(mktemp)
              curl -s -o "$INSTALLER" https://beta.quicklisp.org/quicklisp.lisp
              sbcl --non-interactive --load "$INSTALLER" \
                   --eval "(quicklisp-quickstart:install :path \"$QUICKLISP_HOME\")"
              rm "$INSTALLER"
            fi

            echo "=== <project-name> development environment ==="
            echo "SBCL: $(sbcl --version 2>/dev/null | head -1)"
            echo "Project root: $MCP_PROJECT_ROOT"
          '';
        };
      };
    };
}
```

### 2. .envrc

```bash
use flake
```

### 3. <project-name>.asd

```lisp
(defsystem "<project-name>"
  :version "0.1.0"
  :author ""
  :license "MIT"
  :description ""
  :depends-on ("alexandria")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "main"))
  :in-order-to ((test-op (test-op "<project-name>/tests"))))

(defsystem "<project-name>/tests"
  :depends-on ("<project-name>" "rove")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "main"))
  :perform (test-op (o c)
             (symbol-call :rove :run c)))
```

### 4. src/package.lisp

```lisp
(defpackage #:<project-name>
  (:use #:cl)
  (:export))
```

### 5. src/main.lisp

```lisp
(in-package #:<project-name>)
```

### 6. tests/package.lisp

```lisp
(defpackage #:<project-name>/tests
  (:use #:cl #:rove #:<project-name>))
```

### 7. tests/main.lisp

```lisp
(in-package #:<project-name>/tests)

(deftest test-placeholder
  (ok t "Placeholder test"))
```

### 8. CLAUDE.md

```markdown
# <project-name>

## MCP Integration

This project uses **cl-mcp** for REPL-driven development.

| Tool | Purpose |
|------|---------|
| `repl-eval` | Evaluate Lisp expressions |
| `lisp-read-file` | Read Lisp files with folding |
| `lisp-edit-form` | Structure-aware editing |
| `code-find` | Find symbol definitions |
| `code-describe` | Get symbol metadata |

### Important Rules
- Use `lisp-read-file` instead of `fs-read-file` for `.lisp` files
- Use `lisp-edit-form` instead of `fs-write-file` for existing code
- Always specify package context in `repl-eval`

## Required SubAgents

| Task | SubAgent |
|------|----------|
| Codebase analysis | `cl-codebase-analyst` |
| Macro verification | `cl-macro-verifier` |
| Test execution | `cl-test-runner` |
| Dependency management | `cl-dependency-resolver` |
| Code quality | `cl-linter` |

## Workflow Rules

### Before Creating a PR
Run in parallel:
1. `cl-linter` - mallet quality check
2. `cl-test-runner` - Rove tests
3. `cl-codebase-analyst` - dependency verification

## Project Info

- Test framework: Rove
- Linter: mallet
- Package manager: Quicklisp
```

### 9. .mallet.lisp

```lisp
(:mallet-config
 (:extends :default)
 (:enable :line-length :max 100)
 (:enable :cyclomatic-complexity :max 15)
 (:enable :function-length :max 50)
 (:for-paths ("tests")
   (:enable :line-length :max 120)
   (:disable :unused-variables)))
```

### 10. .gitignore

```
# SBCL
*.fasl
*.lx64fsl
*.dx64fsl

# Quicklisp
quicklisp/

# Nix
result
.direnv/

# Editor
*~
\#*\#
.#*

# OS
.DS_Store
```

### 11. README.md

```markdown
# <project-name>

## Requirements

- Nix with flakes enabled
- direnv (optional but recommended)

## Setup

```bash
# Enter development environment
nix develop

# Or with direnv
direnv allow
```

## Usage

```lisp
(ql:quickload :<project-name>)
```

## Testing

```bash
rove <project-name>.asd
```

## Linting

```bash
mallet src/
```
```

### 12. LICENSE

MIT License (または指定されたライセンス)

## 実行手順

1. ディレクトリ作成
2. 上記ファイルをすべて生成（`<project-name>` を実際の名前に置換）
3. `git init` で初期化
4. `nix flake update` でflake.lockを生成
5. `direnv allow` でdirenv有効化

## 完了後の確認

```bash
cd <project-name>
nix develop
sbcl --eval '(ql:quickload :<project-name>)' --eval '(quit)'
rove <project-name>.asd
mallet src/
```
