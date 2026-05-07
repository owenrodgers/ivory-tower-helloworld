{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  itnSrc = nixpkgs.fetchFromGitHub {
    owner = "HaskellEmbedded";
    repo = "ivory-tower-nix";
    rev = "e2a771e42d5bf668d829e8307b3ae6cf2c533686";
    sha256 = "0mjdwl7ggs7ijb55vm7xjgzp38hi1j9m6apgbsrxdljsh1r5j4d2";
  };

  itn = import itnSrc { inherit compiler; };

  src = itn.pkgs.nix-gitignore.gitignoreSource [] ./.;
in
  itn // {
    shell = itn.mkShell itn.ivorypkgs.ivory-tower-helloworld;
  }
