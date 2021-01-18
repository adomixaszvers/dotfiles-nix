#!/usr/bin/env bash

if [ -e ./current ]; then
    nix run ".#homes.x86_64-linux.$(cat current).activate"
else
    echo "No current environment is set"
fi
