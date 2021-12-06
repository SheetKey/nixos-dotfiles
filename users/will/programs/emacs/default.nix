{ pkgs, ... }:

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
