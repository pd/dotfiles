{ config, pkgs, ... }:
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  homeDir = config.home.homeDirectory;
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
    startWithUserSession = "graphical";
  };

  home.packages = with pkgs; [ emacs-all-the-icons-fonts ];
}
