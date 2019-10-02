{ pkgs ? import <nixpkgs> {} }:
let
  grid-label-deriv =
    { mkDerivation, base, bytestring, containers, filepath, gloss
    , JuicyPixels, optparse-applicative, stdenv, vector
    }:
    mkDerivation {
      pname = "grid-label";
      version = "0.1";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        base bytestring containers filepath gloss JuicyPixels
        optparse-applicative vector
      ];
      homepage = "https://github.com/xc-jp/grid-label#readme";
      license = stdenv.lib.licenses.bsd3;
    };
  grid-label = pkgs.haskellPackages.callPackage grid-label-deriv {};
in
  {
    grid-label-nix = grid-label;
    grid-label-linux = grid-label.overrideAttrs (old: {
      name = old.name + "-linux";
      buildInputs = with pkgs; old.buildInputs ++ [ file patchelf ];
      fixupPhase = ''
        file $out/bin/grid-label
        patchelf --set-interpreter /lib64/ld-linux-x86-64.so.2 $out/bin/grid-label 
        patchelf --set-rpath /lib:/lib/x86_64-linux-gnu:/usr/lib/x86_64-linux-gnu $out/bin/grid-label 
      '';
    });
  }
