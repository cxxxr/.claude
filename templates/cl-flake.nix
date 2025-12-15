{
  description = "Common Lisp project with cl-mcp";

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

      perSystem = { pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            sbcl
            rlwrap
            curl
          ];

          shellHook = ''
            export QUICKLISP_HOME="$HOME/.quicklisp"
            export CL_SOURCE_REGISTRY="${inputs.cl-mcp}/:$PWD//:$QUICKLISP_HOME/local-projects//"

            # Quicklisp自動セットアップ
            if [ ! -f "$QUICKLISP_HOME/setup.lisp" ]; then
              echo "Installing Quicklisp..."
              INSTALLER=$(mktemp)
              curl -s -o "$INSTALLER" https://beta.quicklisp.org/quicklisp.lisp
              sbcl --non-interactive \
                   --load "$INSTALLER" \
                   --eval "(quicklisp-quickstart:install :path \"$QUICKLISP_HOME\")"
              rm "$INSTALLER"
            fi

            export MCP_PROJECT_ROOT="$PWD"
            echo "CL environment ready: SBCL $(sbcl --version 2>/dev/null | head -1)"
          '';
        };
      };
    };
}
