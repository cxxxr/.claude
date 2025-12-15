---
description: プロジェクトにNix Flakeを導入
argument-hint: "<project-type>"
---

# Nix Flake 初期化

対象プロジェクトにNix Flakeを導入してください。

## 引数

- プロジェクトタイプ: `$1`

## 実行手順

1. **プロジェクト構成の確認**
   - 既存のファイル構成を確認
   - 使用言語・フレームワークを特定
   - 既存の `flake.nix` がないか確認

2. **flake.nix の作成**
   - プロジェクトタイプ `$1` に適したテンプレートを使用
   - 以下を含める:
     - `devShells.default`: 開発環境
     - 必要なビルドツール・依存関係
     - 言語固有のツールチェイン

3. **.envrc の作成（direnv用）**
   - `use flake` を設定

4. **.gitignore への追記**
   - `.direnv/` を追加（未設定の場合）

## プロジェクトタイプ別の設定

| タイプ | 主要パッケージ |
|--------|----------------|
| rust | cargo, rustc, rust-analyzer |
| python | python3, pip, venv |
| node | nodejs, npm/pnpm |
| go | go, gopls |
| common-lisp | 下記「Common Lisp向け詳細設定」参照 |
| general | 基本的な開発ツールのみ |

---

## Common Lisp向け詳細設定

Common Lispプロジェクトでは、https://github.com/lem-project/lemのflake.nixを参考にした高度な設定を行う。

### 基本構成

```nix
{
  description = "A flake for <project-name>";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/release-25.11";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      perSystem = { pkgs, ... }:
        let
          lisp = pkgs.sbcl;
        in {
          devShells.default = pkgs.mkShell {
            packages = with pkgs; [
              rlwrap
              lisp
            ];
          };
        };
    };
}
```

### sbcl.buildASDFSystem の使い方

Quicklispにないライブラリや、自作のASDFシステムをビルドする場合：

```nix
mylib = lisp.buildASDFSystem {
  pname = "mylib";
  version = "0.1.0";
  src = ./.;
  systems = [ "mylib" "mylib/tests" ];  # ビルドするシステム名
  lispLibs = with lisp.pkgs; [
    alexandria
    bordeaux-threads
    cl-ppcre
  ];
};
```

### 外部依存ライブラリの管理（nvfetcher）

nixpkgsやQuicklispにないライブラリは、nvfetcherで管理：

1. `nvfetcher.toml` を作成：
```toml
[mylib]
src.git = "https://github.com/user/mylib"
fetch.git = "https://github.com/user/mylib"
```

2. `nvfetcher` コマンドで `_sources/generated.nix` を生成

3. flake.nixで読み込み：
```nix
sources = import ./_sources/generated.nix {
  inherit (pkgs) fetchgit fetchurl fetchFromGitHub dockerTools;
};
mylib = lisp.buildASDFSystem {
  inherit (sources.mylib) pname src version;
  systems = [ "mylib" ];
  lispLibs = with lisp.pkgs; [ alexandria ];
};
```

### Cライブラリ依存がある場合

CFFIでCライブラリを呼び出すシステム用：

```nix
# 1. Cライブラリをビルド
c-mylib = pkgs.stdenv.mkDerivation {
  pname = "c-mylib";
  version = "0.1.0";
  src = ./c-src;
  nativeBuildInputs = with pkgs; [ cmake pkg-config ];
  buildInputs = with pkgs; [ openssl ];
};

# 2. Common Lispバインディング
cl-mylib = lisp.buildASDFSystem {
  pname = "cl-mylib";
  src = ./.;
  systems = [ "cl-mylib" ];
  lispLibs = with lisp.pkgs; [ cffi ];
  nativeLibs = [ c-mylib ];  # Cライブラリへの依存
};
```

### 実行可能バイナリの作成

```nix
myapp = lisp.buildASDFSystem {
  pname = "myapp";
  src = ./.;
  systems = [ "myapp" ];
  lispLibs = with lisp.pkgs; [ alexandria ];
  nativeBuildInputs = with pkgs; [ makeBinaryWrapper ];
  buildScript = pkgs.writeText "build-myapp.lisp" ''
    (load "${myapp.asdfFasl}/asdf.${myapp.faslExt}")
    (asdf:initialize-output-translations
      '(:output-translations :disable-cache :inherit-configuration))
    (asdf:load-system "myapp")
    (setf uiop:*image-entry-point* #'myapp:main)
    (uiop:dump-image "myapp" :executable t :compression t)
  '';
  installPhase = ''
    mkdir -p $out/bin
    install myapp $out/bin
    wrapProgram $out/bin/myapp \
      --prefix LD_LIBRARY_PATH : "$LD_LIBRARY_PATH"
  '';
};
```

### よく使うlispLibs

```nix
lispLibs = with lisp.pkgs; [
  # 基本ユーティリティ
  alexandria        # 汎用ユーティリティ
  iterate           # 強力なループマクロ
  closer-mop        # MOP互換レイヤー
  trivia            # パターンマッチ
  str               # 文字列操作
  split-sequence    # シーケンス分割

  # 並行処理
  bordeaux-threads  # スレッド抽象化
  bt-semaphore      # セマフォ
  chanl             # CSPスタイルチャネル

  # 外部連携
  cffi              # C FFI
  cl-ppcre          # 正規表現
  yason             # JSON
  dexador           # HTTPクライアント
  usocket           # ソケット

  # 開発支援
  log4cl            # ロギング
  swank             # SLIMEバックエンド
];
```

---

## 注意事項

- `flake.nix` が既に存在する場合は上書きせず、差分を提案
- nixpkgs は release-25.11 を使用（安定性重視）
- `flake.lock` は自動生成されるのでコミット推奨
- nvfetcherを使う場合は `_sources/` もコミット推奨
