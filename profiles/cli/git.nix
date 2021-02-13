{ pkgs, ... }: {
  home.packages = with pkgs; [ git-crypt ];
  programs.git = {
    enable = true;
    delta.enable = true;
    signing = {
      signByDefault = true;
      key = "D18136106500F54B412164FACF90DBD10A9B9C26";
    };
    userEmail = "adomixaszvers@gmail.com";
    userName = "Adomas Jatužis";
    extraConfig = {
      merge = { tool = "nvim"; };
      "mergetool \"nvim\"" = { cmd = ''nvim -f -c "Gdiff" "$MERGED"''; };
      credential = { helper = "cache"; };
      github = { user = "adomixaszvers"; };
      pull = { rebase = true; };
    };
  };
}
