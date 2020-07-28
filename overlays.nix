let
  sources = import ./nix/sources.nix;
  hie = import "${sources.all-hies}/overlay.nix";
  nixos-unstable = _: _: {
    nixos-unstable = import sources.nixos-unstable { overlays = [ (import ./mine.nix) hie ]; };
  };
  overlay = _: _:
    import sources.nixpkgs { overlays = [ (import ./mine.nix) hie nixos-unstable ]; };
in [ overlay ]
