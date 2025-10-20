{
  unstable,
  ...
}:
{
  programs.jujutsu = {
    enable = true;
    ediff = false; # it uses emacsclient
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
      merge-tools.nvim = {
        diff-invocation-mode = "file-by-file";
        merge-args = [
          "-f"
          "-d"
          "$output"
          "-M"
          "$left"
          "$base"
          "$right"
          "-c"
          "wincmd J"
          "-c"
          "set modifiable"
          "-c"
          "set write"
        ];
        merge-tool-edits-conflict-markers = true;
        program = "nvim";
      };
      ui = {
        default-command = "log";
        pager = "delta";
        diff-formatter = ":git";
      };
      user = {
        email = "adomixaszvers@gmail.com";
        name = "Adomas Jatužis";
      };
      working-copy.eol-conversion = "input";

    };
  };
}
