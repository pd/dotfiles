{ config, pkgs-unstable, ... }:
let
  homeDir = config.users.users.pd.home;
in
{
  home-manager.users.pd =
    { config, pkgs, ... }:
    let
      inherit (config.lib.file) mkOutOfStoreSymlink;
      lnDot = f: { source = mkOutOfStoreSymlink "${homeDir}/dotfiles/${f}"; };
    in
    {
      home.file = {
        ".emacs.d" = lnDot "emacs.d";
      };

      programs.emacs = {
        enable = true;
        package = pkgs-unstable.emacs30-pgtk;
      };

      services.emacs = {
        enable = true;
        startWithUserSession = true;
      };

      home.packages = with pkgs; [
        nil
        emacsPackages.vterm
      ];
    };
}
