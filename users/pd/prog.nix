{ pkgs, ... }:
{
  home-manager.users.pd = {
    home.packages = with pkgs.unstable; [
      go
      gopls
      gotools
      go-jsonnet
      nil
      (hiPrio ruby) # win over gotools `bundle`
      rubyPackages.pry
      zig
      zig-shell-completions
      zls
    ];
  };
}
