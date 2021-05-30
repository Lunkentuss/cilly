{
  description = "Cilly";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in with pkgs;
      let
        devModifier = drv:
          haskell.lib.addBuildTools drv
          (with pkgs.haskellPackages; [ cabal-install ghcid hpack vhs ]);

        staticOverride = prev: {
          enableStaticLibraries = true ;
          enableSharedExecutables = false;
          enableSharedLibraries = false;
          configureFlags = [
            "--disable-shared"
            "--ghc-option=-optl=-pthread"
            "--ghc-option=-optl=-static"

            # Rant: nixpkgs should really introduce a convention for
            # intializing the static version of shared libraries... but you got
            # to love dontDisableStatic though <3
            "--ghc-option=-optl=-L${glibc.static}/lib"
            "--ghc-option=-optl=-L${zlib.static}/lib"
            "--ghc-option=-optl=-L${gmp6.override { withStatic = true; }}/lib"
            "--ghc-option=-optl=-L${ncurses.override { enableStatic = true; }}/lib"
            "--ghc-option=-optl=-L${libffi.overrideAttrs (prev: { dontDisableStatic = true; })}/lib"
          ];
        };
        args = {
          root = ./.;
          name = "cilly";
        };
        buildMain = isStatic: isDevShell: (
          haskell.lib.overrideCabal
            (pkgs.haskellPackages.developPackage args // (if isDevShell then { returnShellEnv = true; modifier = devModifier; } else {}))
            (if isStatic then staticOverride else lib.id)
        );
        # A statically compiled version cilly
        mainPackageStatic = buildMain true false;
        # A dynamically compiled version of cilly
        mainPackageDynamic = buildMain false false;
        cillyHelper = runCommand "cilly-helper" { } ''
          mkdir -p $out/bin
          cp ${mainPackageStatic}/bin/cilly-helper $out/bin
        '';
      in {
        defaultPackage = mainPackageStatic;
        packages = {
          dynamic = mainPackageDynamic;
          cilly-helper = cillyHelper;
          cilly-helper-container = import ./nix/helper-container.nix {
            inherit pkgs;
            extraPaths = [ cillyHelper ];
          };
          ci = buildPackages.buildEnv {
            name = "ci";
            paths = [ gnused jq skopeo ];
          };
        };
        devShells.default = pkgs.haskellPackages.developPackage (args // {
          returnShellEnv = true;
          modifier = devModifier;
        });
      });
}
