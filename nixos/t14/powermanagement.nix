{ pkgs, ... }: {
  boot = {
    kernelParams = [ "initcall_blacklist=acpi_cpufreq_init" ];
    kernelModules = [ "amd_pstate" ];
  };
  environment.systemPackages = with pkgs; [ cpufrequtils ];
  powerManagement.cpuFreqGovernor = "schedutil";
  services.tlp = {
    enable = true;
    settings = {
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };
}
