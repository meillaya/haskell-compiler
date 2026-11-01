{
  pkgs,
  ...
}:

{
  # A deliberately small Haskell toolchain.  The package set is not pinned
  # here; devenv.yaml pins nixpkgs and `devenv update` moves the shell.
  packages = [
    pkgs.ghc
    pkgs.cabal-install
    pkgs.git
  ];

  enterShell = ''
    echo "ProtoHaskell development shell"
    echo "  ghc:   $(ghc --version)"
    echo "  cabal: $(cabal --version | head -1)"
    echo "  build: cabal build all"
    echo "  test:  cabal test"
  '';

  enterTest = ''
    cabal test --offline
  '';
}
