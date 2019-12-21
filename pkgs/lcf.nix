{ stdenv, fetchurl, luaPackages }:
luaPackages.buildLuarocksPackage {
  pname = "lcf";
  version = "5.3-4";

  src = fetchurl {
    url    = https://luarocks.org/lcf-5.3-4.src.rock;
    sha256 = "1m0kspvwwvkc4z4hr0lwjma1k9x9xlc1w4s5z251mpgqr6d53m0c";
  };
  propagatedBuildInputs = [ luaPackages.lua ];

  meta = with stdenv.lib; {
    homepage = "git://github.com/martin-eden/lua_code_formatter.git";
    description = "Lua code formatter.";
    license = {
      fullName = "GPL v3";
    };
  };
}
