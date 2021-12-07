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
        ;; UI Changes
        (setq inhibit-startup-message t)    ; Disable startup message
        (scroll-bar-mode -1)                ; Disable visible scrollbar
        (tool-bar-mode -1)                  ; Disable the toolbar
        (tooltip-mode -1)                   ; Disable tooltips
        (set-fringe-mode 10)                ; Give breathing room
      '';

      # extra packages for emacs
      usePackage = {

      };

    };
  };
}
