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
      kustomize-sops
      opentofu
      ssm-session-manager-plugin
      stern
    ]
    ++ [ gcloud ];

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
