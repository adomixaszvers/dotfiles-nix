{ pkgs, ... }: {
  # boot = {
  #   kernelParams = [ "initcall_blacklist=acpi_cpufreq_init" ];
  #   kernelModules = [ "amd_pstate" ];
  # };
  environment.systemPackages = with pkgs; [ cpufrequtils ];
  # powerManagement.cpuFreqGovernor = "schedutil";
  services.tlp = {
    enable = true;
    settings = {
      PLATFORM_PROFILE_ON_AC = "balanced";
      PLATFORM_PROFILE_ON_BAT = "low-power";
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };
}
