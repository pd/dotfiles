{ config, ... }:
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
        package = pkgs.unstable.emacs30-pgtk;
        extraPackages =
          epkgs: with epkgs; [
            treesit-grammars.with-all-grammars
            vterm
          ];
      };

      services.emacs = {
        enable = true;
        startWithUserSession = true;
      };

      home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
    };
}
