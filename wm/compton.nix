{ pkgs, config, lib, ... }: {
  services.compton = {
    enable = lib.mkDefault config.xsession.enable;
    shadow = true;
    shadowExclude = [
      "_GTK_FRAME_EXTENTS@:c"
      "class_g = 'Cairo-clock'"
      "class_g = 'Conky'"
      "class_g = 'Firefox' && argb"
      "class_g ?= 'Notify-osd'"
      "class_g ?= 'plasmashell'"
      "class_g ?= 'VirtualBoxVM'"
      "name = 'Notification'"
    ];
    shadowOffsets = [ (-7) (-7) ];
    shadowOpacity = "0.9";
    extraOptions = ''
      shadow-radius = 7;

      inactive-dim = 0.1;
      focus-exclude = [
         "class_g ?= 'plasmashell'"
      ];
    '';
    opacityRule = [
      "95:class_g = 'URxvt' && !_NET_WM_STATE@:32a"
      "0:_NET_WM_STATE@[0]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[1]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[2]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[3]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[4]:32a *= '_NET_WM_STATE_HIDDEN'"
    ];
    vSync = "true";
  };
}
