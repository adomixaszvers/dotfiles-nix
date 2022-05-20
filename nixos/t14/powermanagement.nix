{ pkgs, ... }: {
  boot = {
    kernelParams = [ "initcall_blacklist=acpi_cpufreq_init" ];
    kernelModules = [ "amd_pstate" ];
  };
  environment.systemPackages = with pkgs; [ cpufrequtils ];
  services.tlp.enable = true;
  powerManagement = {
    enable = true;
    cpuFreqGovernor = "schedutil";
  };
}
