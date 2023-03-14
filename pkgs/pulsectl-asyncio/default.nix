{ lib, buildPythonPackage, fetchPypi, pulsectl, setuptools }:
buildPythonPackage rec {
  format = "pyproject";
  pname = "pulsectl-asyncio";
  version = "1.0.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1w2s76wy974mjyfpc9nygiqpydx96680v3wddqcn9c3zl5ribvs6";
  };

  nativeBuildInputs = [ setuptools ];
  propagatedBuildInputs = [ pulsectl ];
}
