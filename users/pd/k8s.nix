{ pkgs, ... }:
let
  # kustomize-sops nixpkg doesn't add it to PATH, but the stack I've inherited
  # certainly expects it to be there
  ksops = pkgs.kustomize-sops.overrideAttrs (
    final: prev: {
      installPhase =
        prev.installPhase
        + ''
          mkdir -p $out/bin
          ln -s $out/lib/viaduct.ai/v1/ksops/ksops $out/bin/ksops
        '';
    }
  );
in
{
  home-manager.users.pd = {
    home.packages =
      [ ksops ]
      ++ (with pkgs; [
        dyff
        kfilt
        kubernetes-helm
        kustomize
        stern
      ])
      ++ (with pkgs.unstable; [
        kind
        kubectl
      ]);

    programs.zsh = {
      initContent = ''
        alias kapf='kubectl apply -f- --server-side --force-conflicts'
        alias kdiff='kubectl diff -f- --server-side'
      '';
    };
  };
}
