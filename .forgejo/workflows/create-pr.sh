#! /usr/bin/env bash

set -eux

branch=update-flake-inputs

if git ls-remote --exit-code origin "refs/heads/$branch"; then
  git fetch origin "$branch:$branch"
  if git diff-index --cached --quiet "origin/$branch"; then
    echo "No changes detected"
    exit
  fi
fi


git push origin "+HEAD:${branch}"

pulls_url="${FORGEJO_API_URL}/repos/${FORGEJO_REPOSITORY}/pulls"
if [ "$(curl --silent "${pulls_url}?state=open"| jq '.|any(.base.label == "master" and .head.label == "update-flake-inputs")')" = false ]; then
  curl -X POST "$pulls_url" -H "Authorization: token ${FORGEJO_TOKEN}" -F "head=$branch" -F 'title=Update flake inputs' -F base=master
fi

