{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "14879bb374e4c02eac42998e9219bc5ef42dddef";
    sha256 = "16n5vplbcd492a9c7dmmy9nkpvsgx29my1hb2pwimq9dkxqd8bna";
  };

  itn = import itnSrc { inherit compiler; };

  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // {
    shell = itn.mkShell itn.ivorypkgs.ivory-tower-helloworld;
  }
