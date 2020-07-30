let
  sources = import ./nix/sources.nix;
  hie = import "${sources.all-hies}/overlay.nix";
  mine = import ./mine.nix;
  nivSources = _: _: { nivSources = sources; };
  nixos-unstable = _: _: {
    nixos-unstable = import sources.nixos-unstable { overlays = [ mine hie ]; };
  };
  overlay = _: _:
    import sources.nixpkgs {
      overlays = [ mine hie nixos-unstable nivSources ];
    };
in [ overlay ]
