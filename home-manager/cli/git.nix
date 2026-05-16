{ pkgs, ... }:
{
  programs = {
    delta = {
      enable = true;
      enableGitIntegration = true;
    };
    git = {
      enable = true;
      package = pkgs.gitFull;
      signing = {
        signByDefault = false;
        key = "D18136106500F54B412164FACF90DBD10A9B9C26";
      };
      settings = {
        core = {
          autocrlf = "input";
        };
        init = {
          defaultBranch = "master";
        };
        merge.tool = "nvimdiff";
        credential = {
          helper = "${pkgs.gitFull}/bin/git-credential-libsecret";
        };
        github = {
          user = "adomixaszvers";
        };
        pull = {
          rebase = true;
        };
        user = {
          email = "adomixaszvers@gmail.com";
          name = "Adomas Jatužis";
        };
      };
      includes = [
        {
          condition = "gitdir:~/.config/nixpkgs/";
          contents.rebase = {
            autoStash = "true";
            autoSquash = "true";
          };
        }
      ];
    };
  };
}
