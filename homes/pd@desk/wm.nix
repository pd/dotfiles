{
  pkgs,
  config,
  lib,
  ...
}:
let
  layout-outputs = lib.getExe' (pkgs.writeShellScriptBin "layout-outputs" ''
    ${pkgs.wlr-randr}/bin/wlr-randr --output DP-1 --mode 3840x2160 --pos 1210,0 --scale 1.25
    ${pkgs.wlr-randr}/bin/wlr-randr --output DP-2 --mode 1920x1200 --pos 0,240  --transform 90
  '') "layout-outputs";
in
{
  home.packages = with pkgs; [
    lswt # to get app-id for riverctl rules
    imv # minimalist image viewer
    river-filtile
    sway-audio-idle-inhibit
    wl-clipboard
    wlr-randr
  ];

  stylix = {
    enable = true;
    polarity = "dark";
    image = config.lib.stylix.pixel "base03";
    base16Scheme = "${pkgs.base16-schemes}/share/themes/ashes.yaml";

    fonts = {
      monospace = {
        name = "FiraCode Nerd Font";
        package = pkgs.fira-code;
      };
      sizes.terminal = 10;
    };

    icons = {
      enable = true;
      package = pkgs.numix-icon-theme;
      dark = "Numix";
      light = "Numix-Light";
    };

    cursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
      size = 24;
    };

    targets = {
      emacs.enable = false;
      firefox.enable = false;
      starship.enable = false;
      waybar = {
        font = "emoji";
      };
    };
  };

  # bluetooth in tray
  services.blueman-applet.enable = true;

  # play/pause with airpod touch
  # nixos wiki says this is 'unnecessary and may cause issues' but it doesn't
  # work without it:
  # https://nixos.wiki/wiki/Bluetooth#Using_Bluetooth_headset_buttons_to_control_media_player
  services.mpris-proxy.enable = true;

  # notifications
  services.dunst = {
    enable = true;
    settings.global = {
      notification_limit = 5;
      monitor = "DP-1";
      origin = "top-right";
      offset = "(10,20)";
    };
  };

  # term
  programs.alacritty = {
    enable = true;
    settings = {
      mouse.hide_when_typing = true;

      # nas doesn't have alacritty in terminfo,
      # not worth fighting
      env.TERM = "xterm-256color";
    };
  };

  # dmenu-y
  programs.fuzzel = {
    enable = true;
    package = pkgs.pd.launcher;
    settings = {
      main.font = lib.mkForce "Noto Sans:size=12";
      key-bindings = {
        execute-or-next = "Control+Tab";
        insert-selected = "Tab";
      };
    };
  };

  # clipboard history
  services.cliphist.enable = true;

  programs.waybar =
    let
      clock = {
        timezones = [
          "America/Chicago"
          "UTC"
        ];
        format = "{:%F %H:%M %Z}";
        tooltip-format = "<tt>{calendar}</tt>";
        calendar = {
          mode = "year";
          mode-mon-col = 3;
          weeks-pos = "";
          on-scroll = 1;
          format =
            with config.lib.stylix.colors.withHashtag;
            let
              span = color: content: "<span color='${color}'>${content}</span>";
              bold = "<b>{}</b>";
              plain = "{}";
            in
            {
              months = span base0C bold;
              days = span base06 plain;
              weekdays = span base08 bold;
              today = span base0E bold;
            };
        };

        actions = {
          on-click-right = "mode";
          on-scroll-up = "tz_up";
          on-scroll-down = "tz_down";
        };
      };

      mode = {
        format = "{text}";
        hide-empty-text = true;
        exec = "${pkgs.pd.waybar-pd}/bin/waybar-pd river-mode";
        return-type = "json";
      };

      pulseaudio =
        let
          pavucontrol = lib.getExe' pkgs.pavucontrol "pavucontrol";
          pactl = lib.getExe' pkgs.pulseaudio "pactl";
          toggle-sink = pkgs.writeShellScript "toggle-sink" ''
            headphones="bluez_output.80_99_E7_D1_87_04.1"
            speakers="alsa_output.pci-0000_2f_00.4.analog-stereo"
            if [[ "$(${pactl} get-default-sink)" == "$speakers" ]]; then
              ${pactl} set-default-sink "$headphones"
            else
              ${pactl} set-default-sink "$speakers"
            fi
          '';
        in
        {
          format = "{icon} {volume}%";
          format-icons = {
            default = "󰓃";
            "bluez_output.80_99_E7_D1_87_04.1" = "";
          };

          on-click = pavucontrol;
          on-click-right = toggle-sink;
          scroll-step = 2;
        };

      tags = {
        num-tags = 6;
        tag-labels = [
          "1:󰿗"
          "2:"
          "3:󰈹"
          "4:"
          "5"
          "6"
        ];
      };

      tray = {
        spacing = 15;
        show-passive-items = true;
      };
    in
    {
      enable = true;

      style = ''
        * {
          font-family: Noto Sans Mono, NotoSans Nerd Font Mono, Symbols Nerd Font;
          font-size: 11pt;
        }

        window#waybar {
          background: transparent;
        }

        .modules-left,
        .modules-center,
        .modules-right {
          background: @base00;
          border-radius: 10px;
          margin: 5px 5px 0 5px;
        }

        .modules-right label {
          margin: 5px;
        }

        #tags .focused {
          box-shadow: inset 0 -2px;
        }

        #tags .occupied {
          color: @base0C;
        }

        #tags .urgent {
          color: @base0F;
        }

        #network.disconnected {
          color: @base0F;
        }

        #custom-dunst, #tray, #custom-river-mode {
          margin: 0 10px;
        }

        #custom-river-mode.reshape {
          color: @base0D;
        }
      '';

      settings.dp-1 = {
        output = "DP-1";

        modules-left = [
          "custom/dunst"
          "tray"
          "custom/river-mode"
        ];

        modules-center = [
          "river/tags"
        ];

        modules-right = [
          "cpu"
          "memory"
          "network"
          "pulseaudio"
          "custom/audio-state"
          "clock"
        ];

        inherit tray clock pulseaudio;
        "river/tags" = tags;
        "custom/river-mode" = mode;

        "custom/dunst" = {
          format = "{icon}";
          format-icons = {
            paused = "󰂛";
            unpaused = "󰂚";
          };
          exec = "${pkgs.pd.waybar-pd}/bin/waybar-pd dunst";
          return-type = "json";
          on-click = "dunstctl history-pop";
          on-click-middle = "dunstctl history-clear";
          on-click-right = "dunstctl set-paused toggle";
        };

        cpu = {
          format = " {usage}%";
          tooltip = false;
        };

        memory = {
          format = " {percentage}%";
          tooltip-format = "";
        };

        network = {
          format-icons = [
            "󰤯"
            "󰤟"
            "󰤢"
            "󰤥"
            "󰤨"
          ];
          family = "ipv4_6";
          format-wifi = "{icon}";
          format-ethernet = "󰈀";
          format-disconnected = "⚠";
          tooltip-format-ethernet = "{ifname}: {ipaddr}";
          tooltip-format-wifi = "{ifname}: {ipaddr} {essid} ({signalStrength}% / {signaldBm})";
          tooltip-format-disconnected = "Disconnected";
        };

        "custom/audio-state" = {
          format = "{icon}";
          exec = "sway-audio-idle-inhibit --dry-print-both-waybar";
          exec-if = "which sway-audio-idle-inhibit";
          return-type = "json";
          format-icons = {
            output = "";
            input = "";
            output-input = "  ";
            none = "";
          };
        };

      };

      settings.dp-2 = {
        output = "DP-2";

        modules-left = [
          "tray"
          "custom/river-mode"
        ];
        modules-center = [ "river/tags" ];
        modules-right = [
          "pulseaudio"
          "clock"
        ];

        inherit tray clock pulseaudio;
        "river/tags" = tags;
        "custom/river-mode" = mode;
      };
    };

  services.swayidle =
    let
      display-state = pkgs.writeShellScript "display-state" ''
        ${pkgs.wlr-randr}/bin/wlr-randr --output DP-1 --"$1" --output DP-2 --"$1"
        if [[ "$1" == "on" ]]; then
          ${layout-outputs}
        fi
      '';
    in
    {
      enable = true;
      systemdTarget = "river-session.target";
      timeouts = [
        {
          timeout = 600;
          command = "${display-state} off";
          resumeCommand = "${display-state} on";
        }
      ];
    };

  wayland.windowManager.river = {
    enable = true;
    package = pkgs.river-classic;
    systemd.enable = true;

    settings =
      let
        mod = "Super";
        tag =
          index:
          let
            i = toString index;
            tags = "$((1 << (${i} - 1)))";
          in
          {
            "${mod} ${i}" = "set-focused-tags ${tags}";
            "${mod}+Shift ${i}" = "set-view-tags ${tags}";
            "${mod}+Control ${i}" = "toggle-focus-tags ${tags}";
            "${mod}+Control+Shift ${i}" = "toggle-view-tags ${tags}";
          };
      in
      {
        declare-mode = [
          "reshape"
        ];

        # New windows are generally subordinate to existing ones, so
        # drop to bottom of the stack
        default-attach-mode = "bottom";

        # Key names:
        # https://github.com/xkbcommon/libxkbcommon/blob/e9fd95/include/xkbcommon/xkbcommon-keysyms.h
        map.normal = {
          "${mod} Return" = "spawn alacritty";
          "${mod} Space" = "spawn '${pkgs.pd.launcher}/bin/fuzzel'";
          "${mod} slash" = "spawn '${pkgs.pd.launcher}/bin/search-menu'";
          "${mod} backslash" = "spawn 'cliphist-fuzzel-img'";

          "${mod} R" = "enter-mode reshape";

          "${mod}+Shift H" = "focus-view left";
          "${mod}+Shift J" = "focus-view down";
          "${mod}+Shift K" = "focus-view up";
          "${mod}+Shift L" = "focus-view right";
          "${mod}+Shift N" = "focus-view next";

          "${mod} comma" = "zoom";
          "${mod}+Shift+Control K" = "swap previous";
          "${mod}+Shift+Control J" = "swap next";

          "${mod}+Shift F" = "toggle-fullscreen";
          "${mod}+Shift Space" = "toggle-float";

          "${mod} braceleft" = "focus-output left";
          "${mod} braceright" = "focus-output right";
          "${mod}+Control braceleft" = "send-to-output left";
          "${mod}+Control braceright" = "send-to-output right";

          "None Print" = "spawn '${pkgs.pd.screenshots}/bin/wl-screenshot-region'";
          "${mod} Print" = "spawn '${pkgs.pd.screenshots}/bin/wl-screenshot-output'";

          "${mod} F12" = "spawn ${layout-outputs}";
          "${mod}+Shift+Control BackSpace" = "exit";
        }
        // (lib.zipAttrs (map tag (lib.range 1 9)));

        map.reshape = {
          "None Escape" = "enter-mode normal";

          "${mod} Z" = "send-layout-cmd filtile flip";

          "${mod} Plus" = "send-layout-cmd filtile 'main-count +1'";
          "${mod} Minus" = "send-layout-cmd filtile 'main-count -1'";

          "Shift Left" = "send-layout-cmd filtile 'move-split-left 0.05'";
          "Shift Up" = "send-layout-cmd filtile 'move-split-up 0.05'";
          "Shift Right" = "send-layout-cmd filtile 'move-split-right 0.05'";
          "Shift Down" = "send-layout-cmd filtile 'move-split-down 0.05'";

          "None Left" = "send-layout-cmd filtile 'main-location left'";
          "None Up" = "send-layout-cmd filtile 'main-location top'";
          "None Right" = "send-layout-cmd filtile 'main-location right'";
          "None Down" = "send-layout-cmd filtile 'main-location bottom'";
        };

        map-pointer.normal = {
          "${mod} BTN_LEFT" = "move-view";
          "${mod} BTN_RIGHT" = "resize-view";
          "${mod} BTN_MIDDLE" = "toggle-float";
        };

        input."pointer-*" = {
          accel-profile = "flat";
          pointer-accel = "0.1";
        };

        rule-add = [
          "ssd"

          "-app-id emacs tags 3" # 1|2
          "-app-id firefox tags 5" # 1|3
          "-app-id Slack tags 8" # 4
          "-app-id signal tags 16" # 5
          "-app-id signal output DP-2"
          "-app-id 'steam_app_*' tags 32" # 6
          "-app-id 'steam_app_*' output DP-1"

          "-app-id .blueman-manager-wrapped float"
          "-app-id imv float"
          "-app-id org.pulseaudio.pavucontrol float"
          "-app-id com.github.PintaProject.Pinta float"
          "-app-id firefox -title Library float"

          # AWS workspaces
          "-app-id Dcvclient tags 16" # 5
          "-app-id Dcvclient output DP-1" # 5
          "-app-id workspacesclient tags 8" # 6
          "-app-id workspacesclient output DP-1"
          "-app-id workspacesclient float"
        ];

        set-cursor-warp = "on-output-change";

        spawn = map (cmd: "'${cmd}'") [
          "waybar"
          "sway-audio-idle-inhibit"
          layout-outputs
        ];

        background-color = lib.mkForce "0x303038";
        border-width = 3;
        default-layout = "filtile";
        focus-follows-cursor = "normal";
      };

    # Ensure send-layout-cmd fires after filtile is actually running
    extraConfig = ''
      # start with focus on main monitor
      riverctl focus-output DP-1

      # on vertical monitor, split vertically and keep new windows small
      # because they're almost always ephemeral
      filtile -view-padding 4 -outer-padding 4 \
        --output DP-2 main-location top, \
        --output DP-2 main-ratio 75 &
    '';
  };
}
