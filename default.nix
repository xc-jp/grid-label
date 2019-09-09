{pkgs ? import (builtins.fetchGit {
  name = "nixos-release-19.03-2019-09-05";
  url = https://github.com/nixos/nixpkgs.git;
  ref = "release-19.03";
  rev = "3d7608eb2673dee9567ddbadab1356444f16c41e";
  }) {
    overlays = [
      (self: super: {
        libGL = self.callPackage ./mesa-stubs.nix {
          inherit (self) OpenGL mesa;
        };
        mesa = self.mesa_noglu;
        freeglut = super.freeglut.overrideAttrs(old: {
            nativeBuildInputs = [super.cmake];
        });
        mesa_noglu = ((super.mesa_noglu.override
            { vulkanDrivers = [];
              galliumDrivers = ["virgl" "nouveau" "vc4" "swrast"];
              driDrivers = ["nouveau"];
            }
          ).overrideAttrs(old: {
            nativeBuildInputs = old.nativeBuildInputs ++ [super.llvm_7];
          }));
        
        xorg = super.xorg // {
          libXi = super.xorg.libXi.overrideAttrs(old: {
            buildInputs = old.buildInputs ++ [super.autoconf super.automake];
            patches = [./libXi-cross-musl.patch];
          });
        };
        grid-label = super.haskell.packages.ghc865.developPackage {
          root = ./.;
        };
      })
    ];
  }
}:
pkgs.pkgsCross.musl64
