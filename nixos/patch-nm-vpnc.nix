{
  nixpkgs.overlays = [
    (_: super: {
      networkmanager-vpnc = super.networkmanager-vpnc.overrideAttrs (old: {
        patches = old.patches ++ [
          ./export_nm_vpn_editor_factory_vpnc.patch
        ];
      });
    })
  ];
}
