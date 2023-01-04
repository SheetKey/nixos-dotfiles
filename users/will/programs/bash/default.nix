# bash default.nix

{ pkgs, ... }:

{

  programs.bash = {
    enable = true;

    bashrcExtra = ''
      export XDG_DATA_HOME="$HOME/.local/share"
    '';

  };
}
