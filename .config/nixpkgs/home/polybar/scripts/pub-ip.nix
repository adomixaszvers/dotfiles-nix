with import <nixpkgs> {};
writeScript "pub-ip.sh" ''
#!${stdenv.shell}

# credits
# https://linuxconfig.org/polybar-a-better-wm-panel-for-your-linux-system

IP=$(${bind}/bin/mdig +short myip.opendns.com @resolver1.opendns.com)

if ${procps}/bin/pgrep -x openvpn > /dev/null; then
    echo VPN: $IP
else
    echo $IP
fi
''
