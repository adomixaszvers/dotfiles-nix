let
  sources = import ./nix/sources.nix;
  mine = import ./pkgs/overlay.nix;
  nivSources = _: _: { nivSources = sources; };
  gitignoreSource = _: super:
    let gitignore = (import sources.gitignore) { inherit (super) lib; };
    in { inherit (gitignore) gitignoreSource; };
  nixos-unstable = _: _: {
    nixos-unstable = import sources.nixos-unstable { overlays = [ mine ]; };
  };
  overlay = _: _:
    import sources.nixpkgs {
      overlays = [ mine nixos-unstable nivSources gitignoreSource ];
    };
in [ overlay ]
