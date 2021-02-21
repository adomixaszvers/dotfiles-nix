{ callPackage, pkg-config, pango, glib, cairo, libxcb, defaultCrateOverrides }:
let
  generatedBuild = callPackage ./Cargo.nix {
    defaultCrateOverrides = defaultCrateOverrides // {
      glib-sys = attrs: {
        nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkg-config ];
        buildInputs = attrs.buildInputs ++ [ glib ];
      };
      cairo-sys-rs = attrs: {
        nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkg-config ];
        buildInputs = attrs.buildInputs ++ [ cairo ];
      };
      gobject-sys = attrs: {
        nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkg-config ];
        buildInputs = attrs.buildInputs ++ [ glib ];
      };
      pango-sys = attrs: {
        nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkg-config ];
        buildInputs = attrs.buildInputs ++ [ pango ];
      };
      pangocairo-sys = attrs: {
        nativeBuildInputs = attrs.nativeBuildInputs ++ [ pkg-config ];
        buildInputs = attrs.buildInputs ++ [ pango ];
      };
    };
  };
in generatedBuild.rootCrate.build.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ [ libxcb ]; })
