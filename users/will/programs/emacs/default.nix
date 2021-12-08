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

        ;; Line numbers
        (column-number-mode)
        (global-display-line-numbers-mode t)
        ;; Disable line numbers in some modes
        (dolist (mode '(org-mode-hook
                        term-mode-hook
                        eshell-mode-hook
                        shell-mode-hook))
          (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

        # Doom-modeline
        doom-modeline = {
          enable = true;
          extraConfig = ''
            :disabled
          '';
        };

        # All the icons
        all-the-icons.enable = true;

        # Rainbow delimiters for elisp
        rainbow-delimiters = {
          enable = true;
          hook = [ "(prog-mode . rainbow-delimiters-mode)" ];
        };

        # Which key
        which-key = {
          enable = true;
          command = [ "which-key-mode" ];
          init = "(which-key-mode)";
          diminish = [ "which-key-mode" ];
          # defer = 1;
          config = ''
            (setq which-key-idle-delay 0.3)
            ;; (which-key-mode)
          '';
        };

        # Vertico for auto completion
        vertico = {
          enable = true;
          command = [ "vertico-mode" ];
          init = "(vertico-mode)";
          bindLocal = {
            vertico-map = { 
              C-j = "vertico-next"; 
              C-k = "vertico-previous";
              C-f = "vertico-exit";
            };
            minibuffer-local-map = {
              M-h = "backward-kill-word";
            };
          };
          extraConfig = ''
            :custom
            (vertico-cycle t)
          '';
        };
        # Savehist for vertico
        savehist = {
          enable = true;
          command = [ "savehist-mode" ];
          init = "(savehist-mode)";
        };
        # Marginalia for vertico extra information
        marginalia = {
          enable = true;
          after = [ "vertico" ];
          command = [ "marginalia-mode" ];
          init = "(marginalia-mode)";
          extraConfig = ''
            :custom
            (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
          '';
        };

        # Helpful: a better help tool
        helpful = {
          enable = true;
          extraConfig = ''
            :custom
            (counsel-describe-function-function #'helpful-callable)
            (counsel-describe-variable-function #'helpful-variable)
            :bind
            ([remap describe-function] . counsel-describe-function)
            ([remap describe-command] . helpful-command)
            ([remap describe-variable] . counsel-describe-variable)
            ([remap describe-key] . helpful-key)
          '';
        };

      };

    };
  };
}
