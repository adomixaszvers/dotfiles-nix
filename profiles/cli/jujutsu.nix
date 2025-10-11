{
  programs.jujutsu = {
    enable = true;
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
        pager = "less --quit-if-one-screen --RAW-CONTROL-CHARS --no-init";
      };
      user = {
        email = "adomixaszvers@gmail.com";
        name = "Adomas Jatužis";
      };

    };
  };
}
