{ unstable, ... }:
{
  programs.jujutsu = {
    enable = true;
    package = unstable.jujutsu;
    settings = {
      aliases.tug = [
        "bookmark"
        "move"
        "--from"
        "heads(::@- & bookmarks())"
        "--to"
        "@-"
      ];
      revset-aliases."private()" = "description(glob:'wip:*') | description(glob:'private:*')";
      git.private-commits = "private()";
      ui = {
        pager = "delta";
        diff-formatter = ":git";
      };
      user = {
        email = "adomixaszvers@gmail.com";
        name = "Adomas Jatužis";
      };

    };
  };
}
