{ pkgs, ... }:

let
  baseQtile = pkgs.qtile;
  unwrapped = baseQtile.unwrapped.overrideAttrs (oldAttrs: {
    propagatedBuildInputs = oldAttrs.propagatedBuildInputs ++ [
      pkgs.python3Packages.xlib
      pkgs.python3Packages.dbus-next
    ];
    buildInputs = oldAttrs.buildInputs ++ [ pkgs.pulseaudio ];
  });
in
(pkgs.python3.withPackages (_: [ unwrapped ])).overrideAttrs (_: {
  # otherwise will be exported as "env", this restores `nix search` behavior
  name = "${unwrapped.pname}-${unwrapped.version}";
  # export underlying qtile package
  passthru = {
    inherit unwrapped;
  };
})
