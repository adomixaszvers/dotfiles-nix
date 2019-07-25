{ pkgs, config, ... }: {
  services.compton = {
    enable = config.xsession.enable;
    shadow = true;
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'Conky'"
      "class_g ?= 'Notify-osd'"
      "class_g = 'Cairo-clock'"
      "_GTK_FRAME_EXTENTS@:c"
    ];
    shadowOffsets = [ (-7) (-7) ];
    shadowOpacity = "0.9";
    extraOptions = ''
      clear-shadow = true;
      shadow-radius = 7;

      inactive-dim = 0.1
    '';
    opacityRule = [
      "95:class_g = 'URxvt' && !_NET_WM_STATE@:32a"
      "0:_NET_WM_STATE@[0]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[1]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[2]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[3]:32a *= '_NET_WM_STATE_HIDDEN'"
      "0:_NET_WM_STATE@[4]:32a *= '_NET_WM_STATE_HIDDEN'"
    ];
    package = pkgs.compton-git;
    vSync = "opengl-swc";
  };
}
