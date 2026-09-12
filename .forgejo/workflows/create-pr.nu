#! /usr/bin/env nix-shell
#! nix-shell -i nu --packages nushell

let branch = 'update-flake-inputs'

git fetch origin $"($branch):($branch)"

if (git diff-index --cached --quiet $"origin/($branch)"| complete).exit_code == 0 {
  exit
}

git push --force $"https://x-access-token:($env.FORGEJO_TOKEN)@git.bl.beastade.top/($env.FORGEJO_REPOSITORY)" $"HEAD:refs/heads/($branch)"
let pullsUrl = $"($env.FORGEJO_API_URL)/repos/($env.FORGEJO_REPOSITORY)/pulls"
let pulls = http get $"($pullsUrl)?state=open"

if ($pulls | where {|x| $x.base.label == master and $x.head.label == $branch and not $x.merged } | is-empty) {
    http post --headers {authorization: $"token ($env.FORGEJO_TOKEN)"} --content-type multipart/form-data $pullsUrl { head: $branch, title: 'Update flake inputs', base: 'master' }
}
