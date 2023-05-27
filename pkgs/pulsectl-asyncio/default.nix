{ lib, buildPythonPackage, fetchPypi, pulsectl, setuptools }:
buildPythonPackage rec {
  format = "pyproject";
  pname = "pulsectl-asyncio";
  version = "1.1.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0ypx7q4nh730ibdwp2l05vc8mv9w09cjmf0r0jahd08fazgf756l";
  };

  patches = [ ./fix_setup_cfg.patch ];

  nativeBuildInputs = [ setuptools ];
  propagatedBuildInputs = [ pulsectl ];
}
