let
  nivSources = import ./nix/sources.nix;
  nix-pre-commit-hooks = import nivSources."pre-commit-hooks.nix";
in {
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    # If your hooks are intrusive, avoid running on each commit with a default_states like this:
    # default_stages = ["manual" "push"];
    hooks = {
      nixfmt.enable = true;
      nix-linter.enable = true;
      shellcheck.enable = true;
    };
  };
}
