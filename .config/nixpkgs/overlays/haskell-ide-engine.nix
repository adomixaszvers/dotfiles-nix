self: super:
let
  hie-nix = super.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "a7ef4c4ceef1dbf46aabff68a4b9bd86d41d0886";
    sha256 = "1hx449l001jc6ijn9zxx30zr1xr2nnrv7qmhpsqwj8wp6s4zyxw8";
  };
in
  with import hie-nix {};
  {
    inherit hies;
  }
