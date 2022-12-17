# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, inputs, ... }: {
  imports = [
    ../avahi.nix
    ../common.nix
    ../flakes.nix
    ../gc.nix
    ../ld-link.nix
    ../libvirtd.nix
    # ../mt7921e-crash-fix.nix
    ../nix-registry.nix
    ../pipewire.nix
    ../syncthing.nix
    ../yubikey.nix
    ../zerotier.nix
    ./dante.nix
    ./hardware-configuration.nix
    ./btrfs.nix
    ./iperf3.nix
    ./ltXkb.nix
    ./prebuild-configs.nix
    ./samba.nix
    ./turbovnc.nix
    # ./unbound.nix
    ./vnc.nix
    ./wireguard-client.nix
    ../fprintd.nix
    # ./throttled
    ./tlp.nix
    inputs.nixos-hardware.nixosModules.common-cpu-intel
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad
    inputs.nixos-hardware.nixosModules.common-pc-laptop-acpi_call
    inputs.nixpkgs.nixosModules.notDetected
    inputs.sops-nix.nixosModules.sops
  ];

  # Use the systemd-boot EFI boot loader.
  boot.cleanTmpDir = true;
  boot.kernelParams = [ "consoleblank=60" ];
  boot.extraModprobeConfig = ''
    options thinkpad_acpi experimental=1 fan_control=1
  '';
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.loader.systemd-boot = {
    enable = true;
    consoleMode = "auto";
  };
  boot.loader.efi.canTouchEfiVariables = true;

  networking = {
    hostId = "d864861a";
    hostName = "adomas-jatuzis-nixos"; # Define your hostname.
    domain = "x.insoft.lt";
    dhcpcd.enable = false;
    networkmanager = {
      enable = true;
      dispatcherScripts = [{
        type = "basic";
        source = pkgs.writeText "wifi-wired-exclusive" ''
          export LC_ALL=C
          PATH=${lib.makeBinPath [ pkgs.networkmanager ]}:$PATH

          enable_disable_wifi ()
          {
              result=$(nmcli dev | grep "ethernet" | grep -w "connected")
              if [ -n "$result" ]; then
                  nmcli radio wifi off
              else
                  nmcli radio wifi on
              fi
          }

          if [ "$2" = "up" ]; then
              enable_disable_wifi
          fi

          if [ "$2" = "down" ]; then
              enable_disable_wifi
          fi
        '';
      }];
    };
  };

  security.pki.certificateFiles = [ ./insoft-ca.crt ];

  environment.systemPackages = with pkgs; [ wget vim virtmanager ];
  environment.variables = {
    VK_ICD_FILENAMES =
      "/run/opengl-driver/share/vulkan/icd.d/intel_icd.x86_64.json";
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.adb.enable = true;
  programs.ssh = {
    extraConfig = ''
      Host github
        HostName github.com
        ProxyCommand nc -x 10.6.0.1:1080 %h %p
    '';
  };
  programs.mosh.enable = true;
  programs.sway.enable = true;
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };
  services.autorandr.enable = true;
  services.gnome.glib-networking.enable = true;
  services.xserver.libinput.enable = true;
  hardware.acpilight.enable = true;

  virtualisation.docker = {
    enable = true;
    storageDriver = "btrfs";
    # listenOptions = [ "/var/run/docker.sock" "2375" ];
  };
  # virtualisation.podman = {
  #   enable = true;
  #   dockerCompat = true;
  #   dockerSocket.enable = true;
  # };
  # virtualisation.containers.storage.settings = {
  #   storage = {
  #     driver = "zfs";
  #     graphroot = "/var/lib/containers/storage";
  #     runroot = "/run/containers/storage";
  #     options.zfs = {
  #       fsname = "rpool/podman";
  #       mountopt = "nodev";
  #     };
  #   };
  # };
  services.gvfs.enable = true;
  services.flatpak = { enable = true; };
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk ];
  };
  services.fstrim = {
    enable = false;
    interval = "monthly";
  };
  services.openssh = {
    enable = true;
    forwardX11 = true;
    extraConfig = ''
      StreamLocalBindUnlink yes
    '';
    passwordAuthentication = false;
    ports = [ 9222 22 ];
  };
  security.pam.services.sshd.enableGnomeKeyring = true;
  services.printing.enable = true;
  services.vnstat.enable = true;

  services.xrdp.enable = true;
  services.xrdp.package = inputs.self.packages.x86_64-linux.custom-xrdp;
  security.pam.services.xrdp-sesman = {
    enableGnomeKeyring = true;
    fprintAuth = false;
  };
  networking.firewall.interfaces.ztzlgoe57z.allowedTCPPorts = [ 3389 ];

  hardware.opengl.driSupport32Bit = true;
  hardware.pulseaudio.support32Bit = true;
  hardware.usbWwan.enable = true;

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "22.05"; # Did you read the comment?

  users.extraGroups.vboxusers.members = [ "adomas" ];

  sops.secrets = {
    "adomas/password" = {
      sopsFile = ./secrets/passwords.yaml;
      neededForUsers = true;
    };
    "root/password" = {
      sopsFile = ./secrets/passwords.yaml;
      neededForUsers = true;
    };
  };

  users.users.adomas.passwordFile = config.sops.secrets."adomas/password".path;
  users.users.root.passwordFile = config.sops.secrets."root/password".path;

  users.users.adomas.extraGroups = [
    "docker"
    "audio"
    "adbusers"
    "libvirtd"
    "qemu-libvirtd"
    "podman"
    "wireshark"
  ];
  users.users.adomas.openssh.authorizedKeys.keyFiles = [
    ../keys/laptop.pub
    ../keys/juice_ed25519.pub
    ../keys/yubikey.pub
    ../keys/t14.pub
  ];

}
