# vim: foldmethod=marker
{
  pkgs,
  lib,
  config,
  ...
}:
{
  imports = [
    ../waybar
    ../dunst.nix
  ];
  home = {
    packages = with pkgs; [
      grimblast
      pamixer
      hyprpicker
      wl-clipboard
      wdisplays
      xwayland
    ];
    sessionVariables = {
      NIXOS_OZONE_WL = 1;
      _JAVA_AWT_WM_NONREPARENTING = 1;
    };
  };
  programs = {
    emacs.package = pkgs.emacs-pgtk;
    waybar = {
      settings.mainbar = {
        layer = "top";
        position = "top";
        height = 16;
        modules-left = [ "ext/workspaces" ];
        modules-center = [ "hyprland/window" ];
        modules-right = (lib.optional config.gui.hasBattery "battery") ++ [
          "hyprland/language"
          "pulseaudio"
          "cpu"
          "memory"
          "temperature"
          "clock"
          "tray"
        ];
        "ext/workspaces".on-click = "activate";
        "hyprland/language" = {
          format-lt = "lt";
          format-en = "us";
        };
        "hyprland/window" = {
          # format = "{title:.100}";
          separate-outputs = true;
        };
        temperature.thermal-zone = config.gui.thermal-zone;
      };
      style = # css
        ''
          window#waybar.fullscreen #window {
            border-radius: 8px;
          }

          /* see https://github.com/Alexays/Waybar/issues/2793#issuecomment-2039369688 */
          #language {
            min-width: 20px;
          }
        '';
      systemd = {
        enable = true;
        targets = [ "hyprland-session.target" ];
      };
    };
  };
  services = {
    hyprpaper.enable = true;
    swayidle = {
      timeouts =
        let
          hyprctl = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl";
        in
        [
          {
            timeout = 360;
            command = "${hyprctl} dispatch dpms off";
            resumeCommand = "${hyprctl} dispatch dpms on";
          }
        ];
    };
  };
  stylix.targets = {
    hyprland.enable = true;
    hyprlock.enable = true;
    hyprpaper.enable = true;
    waybar = {
      enable = true;
      enableCenterBackColors = true;
      enableLeftBackColors = true;
      enableRightBackColors = false;
    };
  };
  wayland.windowManager.hyprland = {
    enable = true;
    configType = "lua";
    xwayland.enable = true;
    settings = {

      layout_bind = {
        _var = lib.generators.mkLuaInline ''
          function (table)
            return function()
              local layout = hl.get_active_workspace().tiled_layout
              if table[layout] then
                hl.dispatch(table[layout])
              elseif table['default'] then
                hl.dispatch(table['default'])
              end
            end
          end
        '';
      };

      # This is an example Hyprland config file.
      #
      # Refer to the wiki for more information.

      #
      # Please note not all available settings / options are set here.
      # For a full list, see the wiki
      #

      # See https://wiki.hyprland.org/Configuring/Monitors/
      monitor = [
        {
          output = "";
          mode = "preferred";
          position = "auto";
          scale = "auto";
        }
      ];

      # See https://wiki.hyprland.org/Configuring/Keywords/ for more

      # Execute your favorite apps at launch
      # exec-once = waybar & hyprpaper & firefox

      # Source a file (multi-file configs)
      # source = ~/.config/hypr/myColors.conf

      # Some default env vars.
      env = [
        {
          _args = [
            "KITTY_CONF_FONT"
            "9.0"
          ];
        }
      ];
      config = {
        # sets xwayland scale

        # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
        input = {
          kb_layout = "lt,us";
          kb_options = "grp:caps_toggle";
          numlock_by_default = true;

          follow_mouse = 1;

          touchpad = {
            natural_scroll = false;
          };

          sensitivity = 0; # -1.0 - 1.0, 0 means no modification.
        };

        xwayland = {
          force_zero_scaling = true;
        };

        general = {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more

          gaps_in = 5;
          gaps_out = 5;
          border_size = 2;

          layout = "scrolling";
          allow_tearing = true;
        };

        decoration = {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more

          rounding = 5;
          blur = {
            enabled = lib.mkDefault true;
            size = 3;
            passes = 1;
            new_optimizations = true;
          };

          shadow = {
            enabled = lib.mkDefault true;
            range = 4;
            render_power = 3;
          };

        };

        dwindle = {
          # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
          preserve_split = true; # you probably want this
        };

        master = {
          # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
          new_status = "slave";
        };

        scrolling.follow_min_visible = 0.6;

        misc = {
          vrr = 1;
          disable_hyprland_logo = true; # no anime
          on_focus_under_fullscreen = 1; # take over
          exit_window_retains_fullscreen = true;
          focus_on_activate = true;
        };
      };

      gesture = [
        # See https://wiki.hyprland.org/Configuring/Variables/ for more
        {
          fingers = 3;
          direction = "horizontal";
          action = "workspace";
        }
      ];

      # Example windowrule v1
      # windowrule = float, ^(kitty)$
      # Example windowrule v2
      # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
      # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

      # See https://wiki.hyprland.org/Configuring/Keywords/ for more

      # Bindings {{{
      # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
      bind =
        let
          mainMod = "SUPER";
          showVolume = ''dunstify -i audio-card -t 2000 -h string:x-dunst-stack-tag:volume "Volume $(pamixer --get-volume)"'';
        in
        [
          {
            _args = [
              "${mainMod} + RETURN"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('kitty')")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + Q"
              (lib.generators.mkLuaInline "hl.dsp.window.close()")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + C"
              (lib.generators.mkLuaInline "hl.dsp.exit()")
            ];
          }
          {
            _args = [
              "${mainMod} + T"
              (lib.generators.mkLuaInline "hl.dsp.window.float()")
            ];
          }
          {
            _args = [
              "${mainMod} + D"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('rofi -show-icons -combi-modi window,drun,run -show combi')")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + D"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('rofi -show run')")
            ];
          }
          {
            _args = [
              "${mainMod} + F"
              (lib.generators.mkLuaInline "hl.dsp.window.fullscreen({mode = 'maximized', action = 'toggle'})")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + F"
              (lib.generators.mkLuaInline "hl.dsp.window.fullscreen({mode = 'fullscreen', action = 'toggle'})")
            ];
          }
          {
            # dwindle
            _args = [
              "${mainMod} + P"
              (lib.generators.mkLuaInline "hl.dsp.window.pseudo()")
            ];
          }
          {
            # dwindle
            _args = [
              "${mainMod} + S"
              (lib.generators.mkLuaInline "hl.dsp.layout('togglesplit')")
            ];
          }
          {
            _args = [
              "${mainMod} + F4"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('rofi-powermenu')")
            ];
          }

          {
            _args = [
              "${mainMod} + W"
              (lib.generators.mkLuaInline "hl.dsp.focus({monitor = 0})")
            ];
          }
          {
            _args = [
              "${mainMod} + E"
              (lib.generators.mkLuaInline "hl.dsp.focus({monitor = 1})")
            ];
          }
          {
            _args = [
              "${mainMod} + R"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('colresize +conf'),
                  default = hl.dsp.focus({monitor = 2}),
                })
              '')
            ];
          }
          {
            _args = [
              "${mainMod} + page_up"
              (lib.generators.mkLuaInline "hl.dsp.focus({workspace = 'm-1'})")
            ];
          }
          {
            _args = [
              "${mainMod} + page_down"
              (lib.generators.mkLuaInline "hl.dsp.focus({workspace = 'm+1'})")
            ];
          }

          {
            _args = [
              "Print"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('grimblast copy output')")
            ];
          }
          {
            _args = [
              "ALT + Print"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('grimblast copy area')")
            ];
          }
          {
            _args = [
              "${mainMod} + SLASH"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('hyprpicker -a')")
            ];
          }

          # Move focus with mainMod + arrow keys
          {
            _args = [
              "${mainMod} + left"
              (lib.generators.mkLuaInline "hl.dsp.focus({direction = 'l'})")
            ];
          }
          {
            _args = [
              "${mainMod} + right"
              (lib.generators.mkLuaInline "hl.dsp.focus({direction = 'r'})")
            ];
          }
          {
            _args = [
              "${mainMod} + up"
              (lib.generators.mkLuaInline "hl.dsp.focus({direction = 'u'})")
            ];
          }
          {
            _args = [
              "${mainMod} + down"
              (lib.generators.mkLuaInline "hl.dsp.focus({direction = 'd'})")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + left"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('swapcol l'),
                  master = hl.dsp.layout('swapnext'),
                })
              '')
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + right"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('swapcol r'),
                  master = hl.dsp.layout('swapprev'),
                })
              '')
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + up"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('consume'),
                })
              '')
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + down"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('expel'),
                })
              '')
            ];
          }

          {
            _args = [
              "${mainMod} + J"
              (lib.generators.mkLuaInline "hl.dsp.layout('cyclenext')")
            ];
          }
          {
            _args = [
              "${mainMod} + K"
              (lib.generators.mkLuaInline "hl.dsp.layout('cycleprev')")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + J"
              (lib.generators.mkLuaInline "hl.dsp.layout('swapnext')")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + K"
              (lib.generators.mkLuaInline "hl.dsp.layout('swapprev')")
            ];
          }
          {
            _args = [
              "${mainMod} + SHIFT + Return"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('promote'),
                  master = hl.dsp.layout('swapwithmaster'),
                })
              '')
            ];
          }
          {
            _args = [
              "${mainMod} + SPACE"
              (lib.generators.mkLuaInline "hl.dsp.layout('orientationcycle left top')")
            ];
          }

          {
            _args = [
              "${mainMod} + C"
              (lib.generators.mkLuaInline "hl.dsp.window.cycle_next({tiled = true})")
            ];
          }

          {
            _args = [
              "${mainMod} + comma"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('colresize -0.1'),
                })
              '')
            ];
          }
          {
            _args = [
              "${mainMod} + period"
              (lib.generators.mkLuaInline ''
                layout_bind({
                  scrolling = hl.dsp.layout('colresize +0.1'),
                })
              '')
            ];
          }

          {
            _args = [
              "${mainMod} + bracketleft"
              (lib.generators.mkLuaInline "hl.dsp.focus({monitor = '-1'})")
            ];
          }
          {
            _args = [
              "${mainMod} + bracketright"
              (lib.generators.mkLuaInline "hl.dsp.focus({monitor = '+1'})")
            ];
          }

          {
            _args = [
              "${mainMod} + CTRL + bracketleft"
              (lib.generators.mkLuaInline "hl.dsp.workspace.swap_monitors({monitor1 = 'current', monitor2 = '-1'})")
            ];
          }
          {
            _args = [
              "${mainMod} + CTRL + bracketright"
              (lib.generators.mkLuaInline "hl.dsp.workspace.swap_monitors({monitor1 = 'current', monitor2 = '+1'})")
            ];
          }

          # Scroll through existing workspaces with mainMod + scroll
          {
            _args = [
              "${mainMod} + mouse_down"
              (lib.generators.mkLuaInline "hl.dsp.focus({workspace = 'e+1'})")
            ];
          }
          {
            _args = [
              "${mainMod} + mouse_up"
              (lib.generators.mkLuaInline "hl.dsp.focus({workspace = 'e-1'})")
            ];
          }

          {
            _args = [
              "${mainMod} + minus"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('pamixer -d 5 && ${showVolume}')")
            ];
          }
          {
            _args = [
              "${mainMod} + equal"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('pamixer -i 5 && ${showVolume}')")
            ];
          }
          {
            _args = [
              "${mainMod} + zcaron"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('pamixer -i 5 && ${showVolume}')")
            ];
          }
          {
            _args = [
              "${mainMod} + F5"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('playerctl play-pause')")
            ];
          }
          {
            _args = [
              "${mainMod} + F6"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('playerctl previous')")
            ];
          }
          {
            _args = [
              "${mainMod} + F7"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('playerctl next')")
            ];
          }

          # Move/resize windows with mainMod + LMB/RMB and dragging
          {
            _args = [
              "${mainMod} + mouse:272"
              (lib.generators.mkLuaInline "hl.dsp.window.drag()")
              { mouse = true; }
            ];
          }
          {
            _args = [
              "${mainMod} + mouse:273"
              (lib.generators.mkLuaInline "hl.dsp.window.resize()")
              { mouse = true; }
            ];
          }
          {
            _args = [
              "XF86AudioLowerVolume"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('pamixer -d 5 && ${showVolume}')")
              { locked = true; }
            ];
          }
          {
            _args = [
              "XF86AudioRaiseVolume"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('pamixer -i 5 && ${showVolume}')")
              { locked = true; }
            ];
          }
          {
            _args = [
              "XF86AudioMute"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('pamixer -t && ${showVolume}')")
              { locked = true; }
            ];
          }
          {
            _args = [
              "XF86AudioPlay"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('playerctl play-pause')")
              { locked = true; }
            ];
          }
          {
            _args = [
              "XF86AudioPrev"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('playerctl previous')")
              { locked = true; }
            ];
          }
          {
            _args = [
              "XF86AudioNext"
              (lib.generators.mkLuaInline "hl.dsp.exec_cmd('playerctl next')")
              { locked = true; }
            ];
          }
        ]
        ++ (builtins.concatMap (
          x:
          let
            ws = toString x;
            keyCode = toString (x + 9);
          in
          [
            {
              _args = [
                "${mainMod} + code:${keyCode}"
                (lib.generators.mkLuaInline "hl.dsp.focus({workspace = ${ws}, on_current_monitor = true})")
              ];
            }
            {
              _args = [
                "${mainMod} + SHIFT + code:${keyCode}"
                (lib.generators.mkLuaInline "hl.dsp.window.move({workspace = ${ws}})")
              ];
            }
          ]
        ) (builtins.genList (x: x + 1) 10));
      # }}}

      # Window rules {{{
      window_rule = [
        {
          name = "windowrule-1";
          float = true;
          match.class = "^(Vampire_Survivors)$";
        }

        {
          name = "windowrule-2";
          workspace = "1 silent";
          match.class = "^(Google-chrome)$";
        }

        {
          name = "windowrule-3";
          workspace = "1 silent";
          match.class = "^(firefox)$";
        }

        {
          name = "windowrule-4";
          workspace = "3 silent";
          match.class = "^(jetbrains-idea)$";
          match.float = false;
        }

        {
          name = "windowrule-5";
          workspace = "5 silent";
          match.class = "^(steam)$";
        }

        {
          name = "windowrule-6";
          workspace = "7";
          match.class = "^(steam_app_3191030)$";
        }

        {
          name = "windowrule-7";
          workspace = "9 silent";
          match.class = "^(KeePassXC)$";
          match.float = false;
        }

        {
          name = "windowrule-8";
          tile = true;
          match.class = "^(com-eviware-soapui-SoapUI)$";
          match.title = "^(SoapUI)(.*)$";
        }

        {
          name = "windowrule-9";
          no_anim = true;
          match.float = "1";
        }

        {
          name = "windowrule-10";
          fullscreen = true;
          match.class = "^(.gamescope-wrapped)$";
        }

        {
          name = "windowrule-11";
          border_size = 0;
          rounding = 0;
          match.float = 0;
          match.workspace = "w[tv1]";
        }

        {
          name = "windowrule-12";
          border_size = 0;
          rounding = 0;
          match.float = 0;
          match.workspace = "f[1]";
        }
      ];
      # }}}

      # for smart gaps
      workspace_rule = [
        {
          workspace = "w[tv1]";
          gaps_out = 0;
          gaps_in = 0;
        }
        {
          workspace = "f[1]";
          gaps_out = 0;
          gaps_in = 0;
        }
        {
          workspace = "3";
          gaps_in = 0;
          gaps_out = 0;
          no_rounding = true;
        }
      ];

      # make workspace animations similar to Niri {{{
      curve = [
        {
          _args = [
            "fluent_decel"
            {
              type = "bezier";
              points = [
                [
                  0
                  0.2
                ]
                [
                  0.4
                  1
                ]
              ];
            }
          ];
        }
      ];
      animation = [
        {
          _args = [
            {
              leaf = "workspaces";
              enabled = true;
              speed = 3;
              bezier = "fluent_decel";
              style = "slidefadevert 30%";
            }
          ];
        }
        {
          _args = [
            {
              leaf = "specialWorkspace";
              enabled = true;
              speed = 2;
              bezier = "fluent_decel";
              style = "slidevert 10%";
            }
          ];
        }
      ];
      # }}}
    };
  };
}
