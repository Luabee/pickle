let pkgsrc = builtins.fetchGit
    {
        url = "https://github.com/nixos/nixpkgs";
        ref = "master";
        rev = "13b815603077abd0bcfcf412f9fbb28df2320ff3";
    };
    pkgs = import pkgsrc {};
in with pkgs;
(haskell.packages.ghc865.callPackage (import ./default.nix) {}).env
