{
  config,
  pkgs,
  inputs,
  lib,
  ...
}:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common.nix
    ../cachyos.nix
    ../flakes.nix
    ../gc.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../realtime.nix
    ../syncthing.nix
    ../yubikey.nix
    ../steam.nix
    ../kde.nix
    # ./wireguard-client.nix
    inputs.nixos-hardware.nixosModules.common-cpu-amd
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia
    inputs.sops-nix.nixosModules.sops
  ];

  boot = {
    initrd.systemd.enable = true;
    loader = {
      systemd-boot = {
        enable = true;
        netbootxyz.enable = true;
        memtest86.enable = true;
      };
      efi.canTouchEfiVariables = true;
    };
    supportedFilesystems = [ "zfs" ];
    kernelPackages = lib.mkDefault pkgs.linuxPackages_6_12;
    kernel.sysctl."vm.max_map_count" = 2147483642;
    zfs.requestEncryptionCredentials = false;
  };

  networking = {
    hostName = "adomo-pc-nixos"; # Define your hostname.
    hostId = "92b8e669";
    networkmanager.enable = true;
  };

  # systemd.tmpfiles.rules = [
  #   "w+ /sys/class/drm/card1/device/power_dpm_force_performance_level - - - - high"
  #   "w+ /sys/class/drm/card1/device/pp_power_profile_mode - - - - 1"
  # ];

  specialisation = {
    # old-kernel.configuration = {
    #   boot = {
    #     kernelPackages = pkgs.linuxPackages_6_1;
    #   };
    # };
  };

  hardware = {
    bluetooth.enable = true;
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    nvidia = {
      open = true;
      prime = {
        offload.enable = false;
        sync.enable = true;
        amdgpuBusId = "PCI:12:0:0";
        nvidiaBusId = "PCI:1:0:0";
      };
      modesetting.enable = true;
      powerManagement.enable = true;
    };
    xone.enable = true;
  };
  # powerManagement.cpuFreqGovernor = "performance";

  services.zfs = {
    autoScrub = {
      enable = true;
      interval = "monthly";
    };
    trim.enable = true;
  };

  programs = {
    gamescope.args = [
      "-r 144"
      "-W 1920 -H 1080"
    ];
  };

  services = {
    flatpak.enable = true;
    fwupd.enable = true;
    gnome.gnome-keyring.enable = false;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    pulseaudio = {
      enable = false;
      support32Bit = true;
      configFile = pkgs.runCommand "default.pa" { } ''
        sed 's/module-udev-detect$/module-udev-detect tsched=0/' \
          ${pkgs.pulseaudio}/etc/pulse/default.pa > $out
      '';
    };
    xserver = {
      enable = true;
      deviceSection = ''
        Option "TearFree" "False"
        Option "VariableRefresh" "True"
        Option "AsyncFlipSecondaries" "true"
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
  ];

  sops.secrets."adomas/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };
  sops.secrets."root/password" = {
    sopsFile = ./secrets/passwords.yaml;
    neededForUsers = true;
  };

  users.users.adomas = {
    hashedPasswordFile = config.sops.secrets."adomas/password".path;
    openssh.authorizedKeys.keyFiles = [
      ../keys/juice_ed25519.pub
      ../keys/yubikey.pub
      ../keys/t14.pub
    ];
  };

  users.users.root.hashedPasswordFile = config.sops.secrets."root/password".path;

  system.stateVersion = "25.05"; # Did you read the comment?

}
