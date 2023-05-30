{
  description = "Nix Setup Repo";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
      };
      in {
        devShell = with pkgs; 
          let vscodeWithExtensions = 
            vscode-with-extensions.override {
              vscodeExtensions = with vscode-extensions; [
                bbenoist.nix
                vscodevim.vim
                esbenp.prettier-vscode
                formulahendry.auto-rename-tag
                yzhang.markdown-all-in-one
                justusadam.language-haskell
                haskell.haskell
              ];
            };
          in 
            mkShell { 
              buildInputs = [ 
                # Web dependencies
                vscodeWithExtensions
              ]; 
            };
      });
}
