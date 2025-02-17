{ pkgs, ... }:

{
  services.grobi = {
    enable = true;
    rules = [
      { name = "projector";
        outputs_connected = ["HDMI-1-1"];
        atomic = true;
        configure_row = ["eDP-1-1" "HDMI-1-1"];
        primary = "eDP-1-1";
      }
      {
        name = "fallback";
        configure_single = "eDP-1-1";
      }
    ];
  };
}
