{ config, pkgs, ... }:
{
  # pretty
  fonts = {
    enableDefaultPackages = true;

    fontconfig = {
      defaultFonts = {
        serif = [ "Noto Serif" ];
        sansSerif = [ "Noto Sans" ];
        emoji = [ "Noto Color Emoji" ];
        monospace = [ "FiraCode Nerd Font" ];
      };
    };
  };

  # make wayland+river+screensharing function
  environment.sessionVariables = {
    NIXOS_OZONE_WL = "1";
    XDG_CURRENT_DESKTOP = "river";
    XDG_SESSION_TYPE = "wayland";
  };

  xdg.portal = {
    enable = true;
    config.common.default = [ "wlr" ];
    wlr = {
      enable = true;
      settings = {
        screencast = {
          chooser_type = "none"; # TODO gotta choose an output now
          output_name = "DVI-I-1";
          max_fps = 60;
        };
      };
    };
  };

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

  # simplest greeter
  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.tuigreet}/bin/tuigreet --time --cmd river";
        user = config.users.users.pd.name;
      };

      initial_session = {
        command = "river";
        user = config.users.users.pd.name;
      };
    };
  };

  # style the console too
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
  };
}
