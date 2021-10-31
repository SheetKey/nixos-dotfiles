# lf default.nix

{
  programs.lf = {
    enable = true;

    extraConfig = (builtins.readFile ./lf);
  };
}
