#! /usr/bin/env bash
HM_PATH=https://github.com/rycee/home-manager/archive/release-18.09.tar.gz
nix-channel --add $HM_PATH home-manager && \
    nix-channel --update home-manager && \
    nix-shell -I home-manager=$HOME/.nix-defexpr/channels/home-manager '<home-manager>' -A install
