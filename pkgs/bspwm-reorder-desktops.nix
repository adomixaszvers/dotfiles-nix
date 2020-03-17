{ writeShellScriptBin }:

writeShellScriptBin "bspwm-reorder-desktops" ''
  _desk_order() {
    while read -r line; do
      printf "%s\\n" "$line"
    done < <(bspc query -D -m "''${1:-focused}" --names) | sort -g | paste -d ' ' -s
  }

  bspc monitor -o $(eval _desk_order) # do not quote! we want term splitting here
''
