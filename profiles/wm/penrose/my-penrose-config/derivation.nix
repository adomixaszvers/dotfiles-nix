{ pkgs }:
let
  customBuildRustCrateForPkgs = pkgs:
    pkgs.buildRustCrate.override {
      defaultCrateOverrides = pkgs.defaultCrateOverrides // {
        glib-sys = _: {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [ pkgs.glib ];
        };
        cairo-sys-rs = _: {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [ pkgs.cairo ];
        };
        gobject-sys = _: {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [ pkgs.glib ];
        };
        pango-sys = _: {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [ pkgs.pango ];
        };
        pangocairo-sys = _: {
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [ pkgs.pango ];
        };
      };
    };
  generatedBuild = import ./Cargo.nix {
    inherit pkgs;
    buildRustCrateForPkgs = customBuildRustCrateForPkgs;
  };
in generatedBuild.rootCrate.build.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ [ pkgs.xorg.libxcb ]; })
