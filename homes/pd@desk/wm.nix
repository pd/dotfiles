{
  pkgs,
  config,
  lib,
  ...
}:
let
  search-menu = pkgs.writeShellScriptBin "search-menu" (builtins.readFile ./search-menu.sh);

  screenshots =
    let
      grim = lib.getExe' pkgs.grim "grim";
      slurp = lib.getExe' pkgs.slurp "slurp";
      wl-copy = lib.getExe' pkgs.wl-clipboard "wl-copy";
    in
    pkgs.symlinkJoin {
      name = "wl-screenshots";
      paths = [
        (pkgs.writeShellScriptBin "wl-screenshot-region" ''
          ${grim} -g "$(${slurp})" - | ${wl-copy}
        '')

        (pkgs.writeShellScriptBin "wl-screenshot-display" ''
          ${grim} - | ${wl-copy}
        '')
      ];
    };
in
{
  home.packages = with pkgs; [
    grim
    lswt # to get app-id for riverctl rules
    imv # minimalist image viewer
    screenshots
    search-menu
    slurp
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
    settings = {
      main.font = lib.mkForce "Noto Sans:size=12";
      key-bindings = {
        execute-or-next = "Control+Tab";
        insert-selected = "Tab";
      };
    };
  };

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

        #tray, #custom-dunst {
          margin: 0 10px;
        }
      '';

      settings.dp-1 = {
        output = "DP-1";

        modules-left = [
          "custom/dunst"
          "tray"
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

        inherit tray clock;
        "river/tags" = tags;

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
          format-wifi = "{icon}";
          format-ethernet = "󰈀";
          format-disconnected = "⚠";
          tooltip-format-wifi = "WiFi: {essid} ({signalStrength}%)\n {bandwidthUpBytes}  {bandwidthDownBytes}";
          tooltip-format-ethernet = "Ethernet: {ifname}\n {bandwidthUpBytes}  {bandwidthDownBytes}";
          tooltip-format-disconnected = "Disconnected";
        };

        pulseaudio = {
          scroll-step = 2;
          on-click = "pavucontrol";
          format = "{icon} {volume}%";
          format-muted = "";
          format-icons = {
            default = [
              ""
              ""
            ];
          };
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

        "custom/dunst" = {
          format = "{text}";
          exec = "${pkgs.pd.waybar-dunst}/bin/waybar-dunst";
          return-type = "json";
          on-click = "dunstctl history-pop";
          on-click-middle = "dunstctl history-clear";
          on-click-right = "dunstctl set-paused toggle";
        };
      };

      settings.dp-2 = {
        output = "DP-2";

        modules-left = [ "tray" ];
        modules-center = [ "river/tags" ];
        modules-right = [ "clock" ];

        inherit tray clock;
        "river/tags" = tags;
      };
    };

  services.kanshi = {
    enable = true;

    settings = [
      {
        output.criteria = "DP-1";
        output.scale = 1.25;
      }
      {

        profile.name = "dual";
        profile.outputs = [
          {
            criteria = "DP-1";
            mode = "3840x2160";
            position = "1210,0";
            scale = 1.25;
          }
          {
            criteria = "DP-2";
            mode = "1920x1200";
            position = "0,240";
            transform = "90";
          }
        ];
      }
    ];
  };

  services.swayidle =
    let
      wlr-randr = lib.getExe' pkgs.wlr-randr "wlr-randr";
      systemctl = lib.getExe' pkgs.systemd "systemctl";

      display-state = pkgs.writeShellScript "display-state" ''
        # TODO: not sure why this suddenly started happening; on resume, river is dead and we're back at greetd --
        # Aug 08 09:20:01 desk swayidle[361952]: 2025-08-08 09:20:01 - [Line 1096] Unable to connect to the compositor.
        # If your compositor is running, check or set the WAYLAND_DISPLAY environment variable.
        #
        # TBD if explicitly setting WAYLAND_DISPLAY will help
        export WAYLAND_DISPLAY=wayland-1

        ${wlr-randr} --output DP-1 --"$1"
        ${wlr-randr} --output DP-2 --"$1"

        # TODO: figure out why positioning is lost when displays come back on
        if [[ "$1" == "on" ]]; then
          ${systemctl} restart --user kanshi
        fi
      '';
    in
    {
      enable = true;
      extraArgs = [ "-d" ];
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

        # Key names:
        # https://github.com/xkbcommon/libxkbcommon/blob/e9fd95/include/xkbcommon/xkbcommon-keysyms.h
        map.normal = {
          "${mod} Return" = "spawn alacritty";
          "${mod} Space" = "spawn 'fuzzel'";
          "${mod} slash" = "spawn '${search-menu}/bin/search-menu'";

          "${mod} R" = "enter-mode reshape";

          "${mod}+Shift H" = "focus-view left";
          "${mod}+Shift J" = "focus-view down";
          "${mod}+Shift K" = "focus-view up";
          "${mod}+Shift L" = "focus-view right";
          "${mod}+Shift N" = "focus-view next";

          "${mod} comma" = "zoom";
          "${mod}+Shift+Control J" = "swap previous";
          "${mod}+Shift+Control K" = "swap next";

          "${mod}+Shift F" = "toggle-fullscreen";
          "${mod}+Shift Space" = "toggle-float";

          "${mod} braceleft" = "focus-output left";
          "${mod} braceright" = "focus-output right";
          "${mod}+Control braceleft" = "send-to-output left";
          "${mod}+Control braceright" = "send-to-output right";

          "None Print" = "spawn 'wl-screenshot-region'";
          "${mod} Print" = "spawn 'wl-screenshot-display'";

          "${mod}+Shift+Control BackSpace" = "exit";
        }
        // (lib.zipAttrs (map tag (lib.range 1 9)));

        map.reshape = {
          "None Escape" = "enter-mode normal";

          "None Plus" = "send-layout-cmd rivertile 'main-count +1'";
          "None Minus" = "send-layout-cmd rivertile 'main-count +1'";

          "Shift Left" = "send-layout-cmd rivertile 'main-ratio -0.05'";
          "Shift Right" = "send-layout-cmd rivertile 'main-ratio +0.05'";

          "None Left" = "send-layout-cmd rivertile 'main-location left'";
          "None Up" = "send-layout-cmd rivertile 'main-location top'";
          "None Right" = "send-layout-cmd rivertile 'main-location right'";
          "None Down" = "send-layout-cmd rivertile 'main-location bottom'";
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

        rule-add =
          let
            tag = app: v: "-app-id '${app}' tags '${toString v}'";
            float = app: "-app-id '${app}' float";
            output = app: output: "-app-id '${app}' output '${output}'";

            tags = lib.mapAttrsToList tag {
              emacs = 3; # 1|2
              firefox = 5; # 1|3
              Slack = 8; # 4
              signal = 16; # 5
            };

            floats = map float [
              ".blueman-manager-wrapped"
              "imv"
              "org.pulseaudio.pavucontrol"
              "com.github.PintaProject.Pinta"
            ];

            outputs = lib.mapAttrsToList output {
              signal = "DP-2";
            };
          in
          [ "ssd" ] ++ tags ++ outputs ++ floats;

        set-cursor-warp = "on-output-change";

        spawn = map (cmd: "'${cmd}'") [
          "waybar"
          "rivertile -view-padding 4 -outer-padding 4"
          "sway-audio-idle-inhibit"
        ];

        background-color = lib.mkForce "0x303038";
        border-width = 3;
        default-layout = "rivertile";
        focus-follows-cursor = "normal";
      };
  };
}
