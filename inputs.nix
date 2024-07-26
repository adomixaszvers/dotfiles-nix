let
  flake =
    if builtins ? getFlake then
      builtins.getFlake ("git+file:" + toString ./.)
    else
      (import ./flakeCompat.nix).defaultNix;
in
{ self = flake; } // flake.inputs
