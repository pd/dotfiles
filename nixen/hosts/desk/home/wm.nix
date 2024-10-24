{
  pkgs,
  config,
  ...
}:
{
  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    liberation_ttf
    nerdfonts
    proggyfonts
  ];

  environment.systemPackages = with pkgs; [
    alacritty
    kitty
    wezterm
  ];
  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  # caps -> ctrl
  services.keyd = {
    enable = true;
    keyboards = {
      default = {
        ids = [ "*" ];
        settings = {
          main = {
            capslock = "layer(control)";
          };
        };
      };
    };
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd Hyprland";
        user = config.users.users.pd.name;
      };

      initial_session = {
        command = "Hyprland";
        user = config.users.users.pd.name;
      };
    };
  };

  programs.hyprland.enable = true;

  stylix = {
    enable = true;
    polarity = "dark";
    image = config.lib.stylix.pixel "base03";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ashes.yaml";
  };

  home-manager.users.pd = {
    programs.wofi.enable = true;

    programs.waybar = {
      enable = true;

      style = ''
        * {
          font-family: FiraCode Nerd Font Mono, Noto Sans Mono;
          font-size: 16px;
        }
      '';

      settings.mainBar = {
        modules-left = [
          "hyprland/workspaces"
          "tray"
        ];

        modules-center = [
          "hyprland/window"
        ];

        modules-right = [
          "cpu"
          "memory"
          "network#lan"
          "network#wg"
          "pulseaudio"
          "clock"
        ];

        "hyprland/workspaces" = {
          active-only = false;
          disable-scroll = true;
          on-click = "activate";

          persistent-workspaces = {
            "1" = [ ];
            "2" = [ ];
            "3" = [ ];
            "4" = [ ];
          };
        };

        cpu = {
          format = " {usage}%";
        };

        memory = {
          format = " {percentage}%";
        };

        "network#lan" = {
          name = "lan";
          format = "<span color='#69ff94'></span> {essid} {signalStrength}%";
          format-disconnected = "<span color='#dd532e'></span>";
          tooltip-format = "{ifname}: {ipaddr}/{cidr} via {gwaddr}";
        };

        "network#wg" = {
          name = "wg";
          format = "<span color='#69ff94'></span>";
          format-disconnected = "<span color='#dd532e'></span>";
          tooltip-format = "{ifname}: {ipaddr}/{cidr} via {gwaddr}";
        };

        clock = {
          timezones = [
            "America/Chicago"
            "UTC"
          ];
          format = " {:%Z|%F %H:%M}";
          tooltip-format = "<tt>{calendar}</tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "";
            on-scroll = 1;
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-scroll-up = "tz_up";
            on-scroll-down = "tz_down";
          };
        };
      };
    };

    wayland.windowManager.hyprland = {
      enable = true;
      settings = {
        monitor = ",preferred,auto,auto";

        exec-once = [
          "waybar &"
        ];

        env = [
          "XCURSOR_SIZE,16"
          "HYPRCURSOR_SIZE,16"
        ];

        general = {
          layout = "dwindle";
          gaps_in = 2;
          gaps_out = 4;
          border_size = 1;
        };

        dwindle = {
          pseudotile = true;
          preserve_split = true;
        };

        master = {
          new_status = "master";
        };

        # misc = {
        #   disable_hyprland_logo = true;
        # };

        input = {
          kb_layout = "us";
          follow_mouse = 0;
        };

        "$mod" = "SUPER";
        "$menu" = "wofi --show drun";

        bind = [
          "$mod, SPACE, exec, $menu"
          "$mod, Return, exec, kitty"
          "$mod, P, pseudo,"
          "$mod, T, togglesplit,"
          "$mod SHIFT, Backspace, exit,"

          "$mod, left, movefocus, l"
          "$mod, right, movefocus, r"
          "$mod, up, movefocus, u"
          "$mod, down, movefocus, d"

          "$mod SHIFT, left, movewindow, l"
          "$mod SHIFT, right, movewindow, r"
          "$mod SHIFT, up, movewindow, u"
          "$mod SHIFT, down, movewindow, d"

          "$mod, 1, workspace, 1"
          "$mod, 2, workspace, 2"
          "$mod, 3, workspace, 3"
          "$mod, 4, workspace, 4"

          "$mod SHIFT, 1, movetoworkspace, 1"
          "$mod SHIFT, 2, movetoworkspace, 2"
          "$mod SHIFT, 3, movetoworkspace, 3"
          "$mod SHIFT, 4, movetoworkspace, 4"
        ];

        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];
      };
    };
  };
}
