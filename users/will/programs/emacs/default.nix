{ pkgs, nur, nixpkgs, ... }:

let 

  nur-no-pkgs = import nur {
    nurpkgs = import nixpkgs { system = "x86_64-linux"; };
  };
  
in
{
  imports = [ nur-no-pkgs.repos.rycee.hmModules.emacs-init ];

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    init = {
      enable = true;

      packageQuickstart = false;
      recommendedGcSettings = true;
      usePackageVerbose = false;


    };
  };
}
