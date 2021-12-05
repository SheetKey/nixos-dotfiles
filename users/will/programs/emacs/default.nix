{ pkgs, ... }:

let 

  nurNoPkgs = import <nur> { pkgs = null; };

in
{
  import = [ nurNoPlgs.repos.rycee.hmModules.programs.emacs.init ];

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
