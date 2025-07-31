{ lib, ... }: {

  services.flatpak ={
    packages = [
      "com.tdameritrade.ThinkOrSwim"
    ];
    update.auto = {
      enable = true;
      onCalendar = "weekly";
    };
  };
}
