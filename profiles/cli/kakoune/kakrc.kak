set-option global tabstop 4
set-option global scrolloff 5,5
set-option global grepcmd 'rg --column'

hook -once global ModuleLoaded fzf %{
    set-option global fzf_file_command 'fd --type f'
    set-option global fzf_highlight_command 'bat'
}

eval %sh{kak-lsp --kakoune -s $kak_session}
hook global WinSetOption filetype=(rust|python|haskell) %{
    lsp-enable-window
    lsp-auto-hover-enable
    lsp-auto-hover-insert-mode-disable
    set-option window lsp_hover_anchor true
    set-option window lsp_hover_max_lines 10
    set-face window DiagnosticError default+u
    set-face window DiagnosticWarning default+u
    map window user l :enter-user-mode<space>lsp<ret>
}
hook global WinSetOption filetype=rust %{
    set-option window lsp_server_configuration rust.clippy_preference="on"
    set-option window formatcmd 'rustfmt'
}
hook global KakEnd .* lsp-exit

hook global WinSetOption filetype=nix %{
    set-option window formatcmd 'nixfmt'
    set-option window lintcmd 'kaknix'
    set-option window tabstop 2
    lint-enable
    lint
    hook window -group lint BufWritePost .*\.nix lint
}

alias global x write-quit
map global user f ':fzf-mode<ret>'
map global insert <c-t> '<a-;><gt>'
map global insert <c-d> '<a-;><lt>'
map global user n ':lint-next-error<ret>'
map global user p ':lint-previous-error<ret>'
map global user c ':comment-line<ret>'
add-highlighter global/ number-lines -relative -hlcursor

# TODO find better solution to ignore x11-* commands
hook global KakBegin .* %{
    evaluate-commands %sh{
        if [ "$TERM" = "xterm-kitty" ] && [ -z "$TMUX" ]; then
            echo 'require-module kitty'
            echo 'alias global terminal kitty-terminal'
            echo 'alias global terminal-tab kitty-terminal-tab'
            echo 'alias global focus kitty-focus'
            echo 'alias global repl kitty-repl'
            echo 'alias global send-text kitty-send-text'
        fi
    }
}
