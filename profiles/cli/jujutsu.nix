{ pkgs, ... }:
{
  home.packages = [ pkgs.lazyjj ];
  programs.jujutsu = {
    enable = true;
    ediff = false; # it uses emacsclient
    settings = {
      aliases.tug = [
        "bookmark"
        "move"
        "--from"
        "heads(::@- & bookmarks())"
        "--to"
        "@-"
      ];
      revset-aliases = {
        "private()" = "description(glob:'wip:*') | description(glob:'private:*')";
        # stack(x, n) is the set of mutable commits reachable from 'x', with 'n'
        # parents. 'n' is often useful to customize the display and return set for
        # certain operations. 'x' can be used to target the set of 'roots' to traverse,
        # e.g. @ is the current stack.
        "stack()" = "stack(@)";
        "stack(x)" = "stack(x, 2)";
        "stack(x, n)" = "ancestors(reachable(x, mutable()), n)";
      };
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
        conflict-marker-style = "snapshot";
      };
      user = {
        email = "adomixaszvers@gmail.com";
        name = "Adomas Jatužis";
      };
      working-copy.eol-conversion = "input";

    };
  };
  programs.zsh.sessionVariables.PURE_GIT_PULL = "0";
}
