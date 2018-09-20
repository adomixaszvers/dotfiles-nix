#! /usr/bin/env bash
HM_PATH=https://github.com/rycee/home-manager/archive/release-18.03.tar.gz
nix-shell $HM_PATH -A install
