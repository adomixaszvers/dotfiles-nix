{
  nix = {
    buildMachines = [
      {
        hostName = "builder";
        # system = "x86_64-linux";
        # if the builder supports building for multiple architectures,
        # replace the previous line by, e.g.,
        systems = [
          "i686-linux"
          "x86_64-linux"
        ];
        maxJobs = 12;
        speedFactor = 2;
        supportedFeatures = [
          "nixos-test"
          "benchmark"
          "big-parallel"
          "kvm"
        ];
        mandatoryFeatures = [ ];
      }
    ];
    distributedBuilds = true;
    # optional, useful when the builder has a faster internet connection than yours
    extraOptions = # ini
      ''
        builders-use-substitutes = true
      '';
  };
}
