{ writers, python3Packages }:
writers.writePython3Bin "tail-volume" {
  libraries = [
    (python3Packages.callPackage ../pulsectl-asyncio {
      inherit (python3Packages) pulsectl;
    })
  ];
} (builtins.readFile ./tail-volume.py)
