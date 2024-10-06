{
  description = "Compiler and tools for the Tiger language.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-21.05";
  };

  outputs = { self, nixpkgs }:
    let
      version = "1.3.0";
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      ocamlPackages = pkgs.ocamlPackages;
      emacsWithPackages = (pkgs.emacsPackagesFor pkgs.emacs-nox).emacsWithPackages;

      emacs = emacsWithPackages (epkgs: [
        epkgs.melpaStablePackages.htmlize
      ]);

      buildInputs = [
        emacs
        pkgs.gcc
        pkgs.cmake
      ] ++ (with ocamlPackages; [
        bos
        cmdliner
        fmt
        lambdasoup
        logs
        menhir
        odoc
				ocaml-lsp
        ppx_expect
        ppx_inline_test
      ]);
    in
    {
      devShell.${system} = pkgs.mkShell {
        buildInputs =
          buildInputs ++ [
            # For clang-format.
            pkgs.clang_11
            pkgs.ocamlformat
          ] ++ (with ocamlPackages; [
            dune_2
            findlib
            # See https://github.com/ocaml/merlin/pull/1284 which was fixed in v4.2.
            (merlin.overrideAttrs (old: {
              version = "4.2-412";

              src = pkgs.fetchurl {
                url = "https://github.com/ocaml/merlin/releases/download/v4.2-412/merlin-v4.2-412.tbz";
                sha256 = "86c30769277d3e2c09a8be6c68a98cd342bc0bdbde07c7225bfe2e6d0da2d394";
              };
            }))
            ocaml
            re
            utop
          ]);
      };

      packages.${system} = {
        fang = ocamlPackages.buildDunePackage {
          useDune2 = true;
          pname = "fang";
          src = self;
          inherit version;
          inherit buildInputs;
          dontUseCmakeConfigure = true;
          doCheck = true;

          postPatch = ''
            substituteInPlace lib/index.mld \
                --replace '%%VERSION%%' ${version};
          '';

          postBuild = "make doc-api";
          outputs = [ "out" "doc" ];

          postInstall = ''
            mkdir -p $doc
            cp -r _build/default/_doc/_html/* $doc/
          '';
        };

        humanDoc = pkgs.stdenv.mkDerivation {
          name = "fang-human-doc-${version}";
          src = self;

          postPatch = ''
            substituteInPlace overview.org \
                --replace '%%VERSION%%' "${version}";
          '';

          buildInputs = [ emacs ];
          buildPhase = "make doc-human";

          installPhase = ''
            mkdir -p $out
            cp overview.html $out/index.html
            cp development.html tiger.html $out/
          '';
        };
      };

      defaultPackage.${system} = self.packages.${system}.fang;
    };
}
