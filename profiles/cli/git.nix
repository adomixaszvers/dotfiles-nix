{ pkgs, ... }: {
  home.packages = with pkgs; [ git-crypt ];
  programs.git = {
    enable = true;
    delta.enable = true;
    signing = {
      signByDefault = false;
      key = "D18136106500F54B412164FACF90DBD10A9B9C26";
    };
    userEmail = "adomixaszvers@gmail.com";
    userName = "Adomas Jatužis";
    extraConfig = {
      core = { autocrlf = "input"; };
      merge = { tool = "nvim"; };
      "mergetool \"nvim\"" = { cmd = ''nvim -f -c "Gdiff" "$MERGED"''; };
      credential = {
        helper = "${pkgs.gitAndTools.gitFull}/bin/git-credential-libsecret";
      };
      github = { user = "adomixaszvers"; };
      pull = { rebase = true; };
    };
  };
}
