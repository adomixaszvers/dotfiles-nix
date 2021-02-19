{ callPackage, pkg-config, pango, glib, cairo, libxcb, defaultCrateOverrides }:
let
  generatedBuild = callPackage ./Cargo.nix {
    defaultCrateOverrides = defaultCrateOverrides // {
      glib-sys = _: {
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ glib ];
      };
      cairo-sys-rs = _: {
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ cairo ];
      };
      gobject-sys = _: {
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ glib ];
      };
      pango-sys = _: {
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ pango ];
      };
      pangocairo-sys = _: {
        nativeBuildInputs = [ pkg-config ];
        buildInputs = [ pango ];
      };
    };
  };
in generatedBuild.rootCrate.build.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ [ libxcb ]; })
