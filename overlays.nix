{self, sources, system}:
let
  hie = import "${sources.all-hies}/overlay.nix";
  mine = self.packages."${system}";
  nivSources = _: _: { nivSources = sources; };
  gitignoreSource = _: super:
    let gitignore = (import sources.gitignore) { inherit (super) lib; };
    in { inherit (gitignore) gitignoreSource; };
  nixos-unstable = _: _: {
    nixos-unstable = import sources.nixos-unstable { inherit system; overlays = [ mine hie ]; };
  };
in [ mine hie nixos-unstable nivSources gitignoreSource ]
