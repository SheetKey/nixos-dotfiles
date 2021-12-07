{ pkgs, nur-no-pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    init = {
      enable = true;

      packageQuickstart = false;
      recommendedGcSettings = true;
      usePackageVerbose = false;

      # early-init.el file
      earlyInit = ''

      '';

      # beginning of init.el
      prelude = ''
        ;; disable startup message
        (setq inhibit-startup-message t)
      '';

      # extra packages for emacs
      usePackage = {

      };

    };
  };
}
