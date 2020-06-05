# This file has been generated by node2nix 1.8.0. Do not edit!

{nodeEnv, fetchurl, fetchgit, globalBuildInputs ? []}:

let
  sources = {
    "@types/commander-2.12.2" = {
      name = "_at_types_slash_commander";
      packageName = "@types/commander";
      version = "2.12.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/commander/-/commander-2.12.2.tgz";
        sha512 = "0QEFiR8ljcHp9bAbWxecjVRuAMr16ivPiGOw6KFQBVrVd0RQIcM3xKdRisH2EDWgVWujiYtHwhSkSUoAAGzH7Q==";
      };
    };
    "@types/diff-3.5.3" = {
      name = "_at_types_slash_diff";
      packageName = "@types/diff";
      version = "3.5.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/diff/-/diff-3.5.3.tgz";
        sha512 = "YrLagYnL+tfrgM7bQ5yW34pi5cg9pmh5Gbq2Lmuuh+zh0ZjmK2fU3896PtlpJT3IDG2rdkoG30biHJepgIsMnw==";
      };
    };
    "@types/get-stdin-5.0.1" = {
      name = "_at_types_slash_get-stdin";
      packageName = "@types/get-stdin";
      version = "5.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/get-stdin/-/get-stdin-5.0.1.tgz";
        sha1 = "46afbcaf09e94fe025afa07ae994ac3168adbdf3";
      };
    };
    "@types/node-14.0.5" = {
      name = "_at_types_slash_node";
      packageName = "@types/node";
      version = "14.0.5";
      src = fetchurl {
        url = "https://registry.npmjs.org/@types/node/-/node-14.0.5.tgz";
        sha512 = "90hiq6/VqtQgX8Sp0EzeIsv3r+ellbGj4URKj5j30tLlZvRUpnAe9YbYnjl3pJM93GyXU0tghHhvXHq+5rnCKA==";
      };
    };
    "commander-2.20.3" = {
      name = "commander";
      packageName = "commander";
      version = "2.20.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/commander/-/commander-2.20.3.tgz";
        sha512 = "GpVkmM8vF2vQUkj2LvZmD35JxeJOLCwJ9cUkugyk2nuhbv3+mJvpLYYt+0+USMxE+oj+ey/lJEnhZw75x/OMcQ==";
      };
    };
    "diff-3.5.0" = {
      name = "diff";
      packageName = "diff";
      version = "3.5.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/diff/-/diff-3.5.0.tgz";
        sha512 = "A46qtFgd+g7pDZinpnwiRJtxbC1hpgf0uzP3iG89scHk0AUC7A1TGxf5OiiOUv/JMZR8GOt8hL900hV0bOy5xA==";
      };
    };
    "get-stdin-5.0.1" = {
      name = "get-stdin";
      packageName = "get-stdin";
      version = "5.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/get-stdin/-/get-stdin-5.0.1.tgz";
        sha1 = "122e161591e21ff4c52530305693f20e6393a398";
      };
    };
    "luaparse-git://github.com/oxyc/luaparse#ac42a00ebf4020b8c9d3219e4b0f84bf7ce6e802" = {
      name = "luaparse";
      packageName = "luaparse";
      version = "0.2.1";
      src = fetchgit {
        url = "git://github.com/oxyc/luaparse";
        rev = "ac42a00ebf4020b8c9d3219e4b0f84bf7ce6e802";
        sha256 = "f813d671f8f8088d70d29f7a7770bdd5ed41ed97240ae9346d7ced0c094ee049";
      };
    };
  };
in
{
  lua-fmt = nodeEnv.buildNodePackage {
    name = "lua-fmt";
    packageName = "lua-fmt";
    version = "2.6.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/lua-fmt/-/lua-fmt-2.6.0.tgz";
      sha1 = "ef9ac0573d1da7330dca09c022c39a33aed347a3";
    };
    dependencies = [
      sources."@types/commander-2.12.2"
      sources."@types/diff-3.5.3"
      sources."@types/get-stdin-5.0.1"
      sources."@types/node-14.0.5"
      sources."commander-2.20.3"
      sources."diff-3.5.0"
      sources."get-stdin-5.0.1"
      sources."luaparse-git://github.com/oxyc/luaparse#ac42a00ebf4020b8c9d3219e4b0f84bf7ce6e802"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "Format Lua code";
      homepage = https://github.com/trixnz/lua-fmt;
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
}