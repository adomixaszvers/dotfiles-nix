#! /usr/bin/env bash

set -eux

branch=update-flake-inputs

if git ls-remote --exit-code origin "refs/heads/$branch"; then
  git fetch origin "$branch:$branch"
fi

if git diff-index --cached --quiet "origin/$branch"; then
  echo "No changes detected"
  exit
fi

server_url="${FORGEJO_SERVER_URL##*/}"
git push --force "https://${FORGEJO_TOKEN}@${server_url}/${FORGEJO_REPOSITORY}" "HEAD:${branch}"

pulls_url="${FORGEJO_API_URL}/repos/${FORGEJO_REPOSITORY}/pulls"
if [ "$(curl --silent "${pulls_url}?state=open"| jq '.|any(.base.label == "master" and .head.label == "update-flake-inputs")')" = false ]; then
  curl -X POST "$pulls_url" -H "Authorization: token ${FORGEJO_TOKEN}" -F "head=$branch" -F 'title=Update flake inputs' -F base=master
fi

