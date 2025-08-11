{
  config,
  pkgs,
  ...
}:
let
  inherit (pkgs.hostPlatform) isDarwin;
  homeDir = config.home.homeDirectory;
  lnDot = f: { source = config.lib.file.mkOutOfStoreSymlink "${homeDir}/dotfiles/${f}"; };
in
{
  home.file = {
    ".emacs.d" = lnDot "emacs.d";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsWithPackagesFromUsePackage {
      package = if isDarwin then pkgs.emacs30 else pkgs.unstable.emacs30-pgtk;
      config = ../../emacs.d/init.el;
      alwaysEnsure = true;
      defaultInitFile = false;
    };
  };

  home.packages = with pkgs; [ emacs-all-the-icons-fonts ];

  services.emacs = {
    enable = true;
    startWithUserSession = "graphical";
  };
}
