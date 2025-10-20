{ pkgs, ... }:
let
  awscli = pkgs.buildEnv {
    name = "awscli";
    paths = with pkgs; [
      awscli2
      eksctl
      ssm-session-manager-plugin
    ];
  };

  gcloud = pkgs.google-cloud-sdk.withExtraComponents [
    pkgs.google-cloud-sdk.components.gke-gcloud-auth-plugin
  ];

  # kustomize-sops nixpkg doesn't add it to PATH, but the stack I've inherited
  # certainly expects it to be there
  ksops = pkgs.kustomize-sops.overrideAttrs (
    final: prev: {
      installPhase = prev.installPhase + ''
        mkdir -p $out/bin
        ln -s $out/lib/viaduct.ai/v1/ksops/ksops $out/bin/ksops
      '';
    }
  );
in
{
  home.packages =
    with pkgs;
    [
      dyff
      eksctl
      kfilt
      unstable.kind
      unstable.kubectl
      kubernetes-helm
      kustomize
      opentofu
      ssm-session-manager-plugin
      stern
    ]
    ++ [
      gcloud
      ksops
    ];

  programs.awscli = {
    enable = true;
    package = awscli;
    settings = {
      default = {
        region = "us-east-2";
        output = "json";
      };
    };
  };

  programs.zsh = {
    initContent = ''
      alias kapf='kubectl apply -f- --server-side --force-conflicts'
      alias kdiff='kubectl diff -f- --server-side'
    '';

    # awscli zsh completion doesn't autoload as expected, dunno why
    completionInit = ''
      complete -C ${awscli}/bin/aws_completer aws
    '';
  };
}
