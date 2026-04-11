{
  programs.ssh.knownHostsFiles = [
    (builtins.path {
      path = ./keys/github.pub;
      name = "github-hosts";
    })
  ];
}
