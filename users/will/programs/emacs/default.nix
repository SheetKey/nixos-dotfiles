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

        (require 'doom-modeline)
        (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
        (doom-modeline-mode 1)
      '';

      # beginning of init.el
      prelude = ''
        ;; UI Changes
        (setq inhibit-startup-message t)    ; Disable startup message
        (scroll-bar-mode -1)                ; Disable visible scrollbar
        (tool-bar-mode -1)                  ; Disable the toolbar
        (tooltip-mode -1)                   ; Disable tooltips
        (set-fringe-mode 10)                ; Give breathing room
        (menu-bar-mode -1)                  ; Disable menu bar

        ;; Set up visual bell
        (setq visible-bell t)

        ;; Font
        (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 100)

        ;; TEMPORARY THEME
        (load-theme 'wombat)

        ;; Make ESC quit prompts
        (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
      '';

      # extra packages for emacs
      usePackage = {
        # ivy buffer completion
        ivy = {
          enable = true;
          diminish = [ "ivy-mode" ];
          bind = {
            "C-s" = "swiper";
          };
          config = ''
            (ivy-mode 1)
          '';
          extraConfig = ''
            :bind ( :map ivy-minibuffer-map
                    ("TAB" . ivy-alt-done)
                    ("C-l" . ivy-alt-done)
                    ("C-j" . ivy-next-line)
                    ("C-k" . ivy-previous-line)
                    :map ivy-switch-buffer-map
                    ("C-k" . ivy-previous-line)
                    ("C-l" . ivy-done)
                    ("C-d" . ivy-switch-buffer-kill)
                    :map ivy-reverse-i-search-map
                    ("C-k" . ivy-previous-line)
                    ("C-d" . ivy-reverse-i-search-kill)
                  )
          '';
        };

        # Doom-modeline
        doom-modeline = {
          enable = true;
          extraConfig = ''
            :disabled
          '';
        };

        # All the icons
        all-the-icons.enable = true;
        # all-the-icons-dired = {
        #   enable = true;
        #   after = [ "dired" "all-the-icons" ];
        #   hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
        # };

      };

    };
  };
}
