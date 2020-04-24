#! /usr/bin/env sh

RELEASE=20.03
HM_PATH=https://github.com/rycee/home-manager/archive/release-${RELEASE}.tar.gz
nix-channel --add $HM_PATH home-manager && \
    nix-channel --update home-manager && \
    nix-shell -I home-manager="$HOME/.nix-defexpr/channels/home-manager" '<home-manager>' -A install
