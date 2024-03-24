{ writers, python3Packages }:
writers.writePython3Bin "tail-volume" {
  libraries = [ python3Packages.pulsectl-asyncio ];
} (builtins.readFile ./tail-volume.py)
