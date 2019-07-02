{ local-anduril-nixpkgs ? false
, local ? false
}:
let bootpkgs = import <nixpkgs> {};
    pkgsrc = builtins.fetchGit
    {
        url = "git@github.com:narsil-reforged/anduril-nixpkgs.git";
        rev = "25e331bd330290a3b1640f47c96a034e6666bf12";
        ref = "master";
    };
    pkgs = if (local-anduril-nixpkgs || local)
           then (if bootpkgs ? anduril
                 then bootpkgs
                 else throw noAndurilMsg
                )
           else import pkgsrc {};
    noAndurilMsg = ''
An environment built from a local anduril-nixpkgs checkout was requested, but
the nixpkgs checkout referred to by NIX_PATH does not contain 'anduril'. Check
that anduril-nixpkgs is checked out locally and NIX_PATH is set correctly. See:

https://github.com/narsil-reforged/anduril-nixpkgs
'';
in with pkgs;
(anduril.localSrc anduril.pickle).env
