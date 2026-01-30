# https://docs.jj-vcs.dev/latest/FAQ/#can-i-monitor-how-jj-log-evolves
{
  jujutsu,
  watchexec,
  lib,
  writers,
}:
writers.writeDashBin "jj-watch" ''
  exec ${lib.getExe watchexec} --quiet --clear --restart --watch=.jj/repo/op_heads/heads --ignore-nothing --wrap-process=none -- ${lib.getExe jujutsu} --ignore-working-copy log
''
