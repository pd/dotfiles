{ pkgs, ... }:
{
  home-manager.users.pd = {
    home.packages =
      with pkgs;
      [
        dyff
        kfilt
        kubernetes-helm
        kustomize
        kustomize-sops
        stern
      ]
      ++ (with unstable; [
        kind
        kubectl
      ]);

    programs.zsh = {
      envExtra = ''
        # kustomize-sops nixpkg doesn't add it to PATH, but the stack I've inherited
        # certainly expects it to be there
        export PATH="${pkgs.kustomize-sops}/lib/viaduct.ai/v1/ksops:$PATH"
      '';

      initContent = ''
        alias kapf='kubectl apply -f- --server-side --force-conflicts'
        alias kdiff='kubectl diff -f- --server-side'
      '';
    };
  };
}
