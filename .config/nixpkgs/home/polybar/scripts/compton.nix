with import <nixpkgs> {};
writeScript "compton.sh" ''
#!${stdenv.shell}

# https://github.com/jaagr/polybar/wiki/User-contributed-modules

#The icon that would change color
icon=""

if ${procps}/bin/pgrep -x "compton" > /dev/null
then
	echo "%{F#00AF02}$icon " #Green
else
	echo "%{F#65737E}$icon " #Gray
fi
''
