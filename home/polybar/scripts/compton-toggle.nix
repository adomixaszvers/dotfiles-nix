{ pkgs}:
with pkgs;
writeScript "compton-toggle.sh" ''
#!${stdenv.shell}

# https://github.com/jaagr/polybar/wiki/User-contributed-modules

#The command for starting compton
#always keep the -b argument!

if ${procps}/bin/pgrep -x "compton" > /dev/null
then
	${psmisc}/bin/killall compton
else
	${compton}/bin/compton -b --config ~/.config/i3/compton.conf
fi
''
