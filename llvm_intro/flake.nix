{
  description = "Compiler and tools for the Tiger language.";

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
          pkgs.gcc
        ]
        ++ (with ocamlPackages; [
          llvm_14
          bos
          fmt
          ocaml-lsp
        ]);
    in {
      devShell.${system} = pkgs.mkShell {
        buildInputs =
          buildInputs
          ++ [
            # For clang-format.
            pkgs.clang_14
            pkgs.ocamlformat
          ]
          ++ (with ocamlPackages; [
            dune_2
            findlib
            ocaml
            re
            utop
          ]);
      };

      packages.${system} = {
        fang = ocamlPackages.buildDunePackage {
          useDune2 = true;
          pname = "cjames_llvm_intro";
          src = self;
          inherit buildInputs;
          dontUseCmakeConfigure = true;
          doCheck = true;
        };
      };
      defaultPackage.${system} = self.packages.${system}.fang;
    });
}
