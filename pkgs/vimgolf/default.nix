{ bundlerApp }:

bundlerApp {
  pname = "vimgolf";
  gemdir = ./.;
  exes = [ "vimgolf" ];
}
