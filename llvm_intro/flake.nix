{
  description = "Colin Jame's demo tiny LLVM compiler";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = import nixpkgs {inherit system;};
      ocamlPackages = pkgs.ocamlPackages;
      buildInputs =
        [
          pkgs.llvm
        ]
        ++ (with ocamlPackages; [
          ocaml
          bos
          fmt
          llvm
        ]);
    in {
      devShell = pkgs.mkShell {
        buildInputs =
          buildInputs
          ++ [
            # For clang-format.
            pkgs.clang
            pkgs.ocamlformat
          ]
          ++ (with ocamlPackages; [
            dune_3
            findlib
            ocaml-lsp
            re
            utop
          ]);
      };

      packages = {
        fang = ocamlPackages.buildDunePackage {
          useDune2 = true;
          pname = "llvm_intro";
          src = self;
          inherit buildInputs;
          dontUseCmakeConfigure = true;
          doCheck = true;
        };
      };
      defaultPackage = self.packages.${system}.fang;
    });
}
