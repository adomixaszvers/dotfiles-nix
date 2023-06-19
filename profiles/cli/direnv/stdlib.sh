: "${XDG_CACHE_HOME:="${HOME}/.cache"}"
declare -A direnv_layout_dirs
direnv_layout_dir() {
    local dhash path
    echo "${direnv_layout_dirs[$PWD]:=$(
        dhash="$(sha1sum - <<< "$PWD" | cut -d' ' -f1)"
        path="${PWD//[^a-zA-Z0-9]/-}"
        echo "${XDG_CACHE_HOME}/direnv/layouts/${dhash}${path}"
    )}"
}
