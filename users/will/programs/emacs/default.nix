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
        (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 110)

        ;;;;;;;;;;; KEYBINDS
        ;; Make ESC quit prompts
        (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

        ;;;;;;;;;;; Function for evil mode
        (defun will/evil-hook ()
          (dolist (mode '(;custom-mode
                          eshell-mode
                          ; git-rebase-mode
                          ; erc-mode
                          ; circe-server-mode
                          ; circe-chat-mode
                          ; circe-query-mode
                          ; sauron-mode
                          ; term-mode
                          ))
          (add-to-list 'evil-emacs-state-modes mode)))

          ;;;;;;;;;; Org-mode hook function
          (defun will/org-mode-setup ()
            (org-indent-mode)
            (variable-pitch-mode 1)
            ;; (auto-fill-mode 0)
            (visual-line-mode 1)
            (setq evil-auto-indent nil))

          ;;;;;;;;;; Org-mode heading sizes function
          (defun will/org-font-setup ()
            (dolist (face '((org-level-1 . 1.5)
                            (org-level-2 . 1.4)
                            (org-level-3 . 1.3)
                            (org-level-4 . 1.2)
                            (org-level-5 . 1.3)
                            (org-level-6 . 1.3)
                            (org-level-7 . 1.3)
                            (org-level-8 . 1.3)))
                    (set-face-attribute (car face) nil :font "Hack Nerd Font Mono" :weight 'bold :height (cdr face))))

          ;;;;;;;;;; Org-mode visual fill
          (defun will/org-mode-visual-fill ()
            (setq visual-fill-column-width 100
                  visual-fill-center-text t)
            (visual-fill-column-mode 1))
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

        # Colorscheme
        doom-themes = {
          enable = true;
          config = ''
            (setq doom-themes-enable-bold t
                  doom-themes-enable-italic t)
            (load-theme 'doom-solarized-dark t)
          '';
        };
        doom-themes-ext-visual-bell = {
          enable = true;
          after = [ "doom-themes" ];
          config = "(doom-themes-visual-bell-config)";
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
          config = ''
            (setq history-length 25)
            (savehist-mode 1)
          '';
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
        # Consult with vertico
        consult = {
          enable = true;
          demand = true;
          bind = {
            "C-s" = "consult-line";
            "C-x b" = "consult-buffer";
            "M-g M-g" = "consult-goto-line";
            "M-g g" = "consult-goto-line";
            "M-s f" = "consult-find";
            "M-s r" = "consult-ripgrep";
          };
          bindLocal = {
            minibuffer-local-map =
              { C-r = "consult-history"; };
            };
        };

        # Helpful: a better help tool
        helpful = {
          enable = true;
          extraConfig = ''
            :bind
            ;; remap default describe functions to helpful describe functions
            ([remap describe-function] . helpful-function)
            ([remap describe-symbol] . helpful-symbol)
            ([remap describe-command] . helpful-command)
            ([remap describe-variable] . helpful-variable)
            ([remap describe-key] . helpful-key)
          '';
        };

        # General keybinds manager
        general = {
          enable = true;
          config = ''
            (general-create-definer will/leader-keys
              :keymaps '(normal insert visual emacs)
              :prefix "SPC"
              :global-prefix "C-SPC")
          '';
        };

        # Evil mode
        evil = {
          enable = true;
          init = ''
            (setq evil-want-integration t)
            (setq evil-want-keybinding nil)
            (setq evil-want-C-u-scroll t)
            (setq evil-want-C-i-jump nil)
          '';
          # hook = [ "(evil-mode . will/evil-hook)" ];
          config = ''
            (evil-mode 1)
            (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
            (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

            ;; Use visual line motions even outside of visual-line-mode buffers
            (evil-global-set-key 'motion "j" 'evil-next-visual-line)
            (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

            (evil-set-initial-state 'messages-buffer-mode 'normal)
            (evil-set-initial-state 'dashboard-mode 'normal)
          '';
        };
        evil-collection = {
          enable = true;
          after = [ "evil" ];
          config = "(evil-collection-init)";
        };

        projectile = {
          enable = true;
          diminish = [ "projectile-mode" ];
          config = "(projectile-mode)";
          bindKeyMap = { "C-c p" = "projectile-command-map"; };
          init = ''
            (when (file-directory-p "~/Documents/Projects")
              (setq projectile-project-search-path '("~/Documents/Projects")))
            (setq projectile-switch-project-action #'projectile-dired)
          '';
        };
        
	      # Nix-mode for emacs
	      nix-mode = {
	        enable = true;
	        extraConfig = ''
	          :mode "\\.nix\\'"
	        '';
	      };
        nix-flake = {
          enable = true;
          after = [ "nix-mode" ];
        };

        # Magit
        magit = {
          enable = true;
          command = [ "magit-status magit-get-current-branch" ];
          extraConfig = ''
            :custom
            (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
          '';
        };
        ssh-agency = {
          enable = true;
        };

        # Org mode
        org = {
          enable = true;
          hook = [ "(org-mode . will/org-mode-setup)" ];
          config = ''
            (setq org-ellipsis " ▾"
                  org-hide-emphasis-markers t)

            (setq org-agenda-start-with-log-mode t)
            (setq org-log-done 'time)
            (setq org-log-into-drawer t)

            (setq org-agenda-files
                  '("~/Documents/Org/Tasks.org"
                    "~/Documents/Org/Birthdays.org"))

            (setq org-todo-keywords
                  '((sequence "TODO(t)" "CURRENT(c)" "NEXT(n)" "|" "DONE(d!)")))

            (font-lock-add-keywords 'org-mode
                                    '(("^ *\\([-]\\) "
                                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

            (setq org-refile-targets
                  '(("Archive.org" :maxlevel . 1)))
            (advice-add 'org-refile :after 'org-save-all-org-buffers)

            (will/org-font-setup)
          '';
        };
        org-bullets = {
          enable = true;
          hook = [ "(org-mode . org-bullets-mode)" ];
        };
        # visual-fill-column = {
        #   enable = true;
        #   defer = true;
        #   hook = [ "(org-mode . will/org-mode-visual-fill)" ];
        # };

      };

    };
  };
}
