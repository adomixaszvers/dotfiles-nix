function hcd
    cd ~/.config/nixpkgs || exit 1
end

function he
    if pushd ~/.config/nixpkgs
        set -l FILES (fzf --print0 --multi | string split0)
        set -q FILES[1] && $EDITOR $FILES
        popd
    end
end
