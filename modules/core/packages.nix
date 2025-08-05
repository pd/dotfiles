{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    curl
    dig
    fd
    git
    htop
    jq
    nvd # to diff nixos generations
    procps
    ripgrep
    tcpdump
    tree
    vim
  ];
}
