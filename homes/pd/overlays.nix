{ inputs, system }:
let
  isDarwin = system == "x86_64-darwin" || system == "aarch64-darwin";
in
{
  nixpkgs.overlays =
    if isDarwin then
      # On darwin, use the emacs from nix-darwin-emacs and packages from
      # emacs-overlay.
      [
        inputs.nix-darwin-emacs.overlays.emacs
        inputs.emacs-overlay.overlays.package
      ]
    else
      # On linux, use emacs-overlay for the packages and FromUsePackage
      # helper, but we run an emacs from nixpkgs-unstable.
      [
        inputs.emacs-overlay.overlays.default
      ];
}
