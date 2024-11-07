{ pkgs, ... }:
{
  environment.systemPackages = with pkgs; [
    curl
    dig
    fd
    git
    htop
    jq
    procps
    ripgrep
    tcpdump
    tree
    vim
  ];
}
