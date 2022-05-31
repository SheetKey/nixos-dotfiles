{ pkgs, nur-no-pkgs, ... }:

{
  programs.emacs = {
    enable = true;
    package = pkgs.emacsGitNativeComp;
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
        (tab-bar-mode 0)
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

        ;; Backup file
        (setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
          backup-by-copying t    ; Don't delink hardlinks
          version-control t      ; Use version numbers on backups
          delete-old-versions t  ; Automatically delete excess backups
          kept-new-versions 20   ; how many of the newest versions to keep
          kept-old-versions 5    ; and how many of the old
        )
        ;; Autosaves
        (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

        ;; Line numbers
        (column-number-mode)
        (global-display-line-numbers-mode t)
        ;; Disable line numbers in some modes
        (dolist (mode '(org-mode-hook
                        term-mode-hook
                        eshell-mode-hook
                        shell-mode-hook
                        vterm-mode-hook))
          (add-hook mode (lambda () (display-line-numbers-mode 0))))

        ;; Set up visual bell
        (setq visible-bell t)

        ;; Font
        (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 110)
        (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono")
        (set-face-attribute 'variable-pitch nil :font "TeX Gyre Schola")

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
          ;; (variable-pitch-mode 1)
          ;; (auto-fill-mode 0)
          (visual-line-mode 1)
          (setq evil-auto-indent nil))

        ;;;;;;;;;; Org-mode font function
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

        ;;;;;;;; Octave mode
        (setq auto-mode-alist
              (cons '("\\.m$" . octave-mode) auto-mode-alist))

        (add-hook 'octave-mode-hook
                  (lambda ()
                    (abbrev-mode 1)
                    (auto-fill-mode 1)
                    (if (eq window-system 'x)
                        (font-lock-mode 1))))

        ;;;;;;;; LaTeX (AucTeX)
        (load "auctex.el" nil t t)
        (load "preview-latex.el" nil t t)

        (setq TeX-auto-save t
              TeX-parse-self t
              TeX-electric-math (cons "$" "$")
              LaTeX-electric-left-right-brace t
              TeX-electric-sub-and-superscript t
              LaTeX-math-menu-unicode t
              TeX-insert-macro-default-style 'show-optional-args
        )
        (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
        (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
        
        ;;;;;;;; RefTeX
        (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
        (setq reftex-plug-into-AUCTeX t)

        ;;;;;;; Tab bar mode
        (setq ;;tab-bar-mode t
              tab-bar-show t
              tab-bar-new-tab-choice "*scratch*"
              tab-bar-new-tab-to 'rightmost
              tab-bar-close-button-show nil
              tab-bar-new-button-show nil
        )

        ;;(add-hook 'tab-new (call-interactively #'tab-bar-rename-tab))
        (add-hook 'tab-bar-tab-post-open-functions (lambda (&rest_) (call-interactively #'tab-bar-rename-tab)))
        
        ;; Func for exwm
        (defun will/exwm-update-class ()
          (exwm-workspace-rename-buffer exwm-class-name))
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
        # completion style for vertico
        orderless = {
          enable = true;
          config = ''
            (setq completion-styles '(orderless)
            )
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
            (setq evil-want-C-u-scroll nil)
            (setq evil-want-C-i-jump nil)
            (setq evil-undo-system 'undo-fu)
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
        evil-nerd-commenter = {
          enable = true;
          extraConfig = ''
            :bind ("M-/" . evilnc-comment-or-uncomment-lines)
          '';
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
        undo-fu = {
          enable = true;
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
          command = [ "magit-status" "magit-get-current-branch" ];
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
          # package = epkgs: epkgs.org-plus-contrib;
          hook = [ "(org-mode . will/org-mode-setup)" ];
          config = ''
            (setq org-ellipsis " ▾"
                  org-hide-emphasis-markers t)

            (setq org-todo-keywords
                  '((sequence "TODO(t)" "CURRENT(c)" "NEXT(n)" "REVIEW(r)" "|" "DONE(d!)"))
                  org-todo-keyword-faces
                  '(("TODO" . org-todo)
                    ("CURRENT" . org-todo)
                    ("NEXT" . org-todo)
                    ("REVIEW" . org-done)
                    ("DONE" . org-done))
            )

            (font-lock-add-keywords 'org-mode
                                    '(("^ *\\([-]\\) "
                                       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

            (org-babel-do-load-languages
                'org-babel-load-languages
                '((emacs-lisp . t )
                  (shell . t)
                  (mathematica . t)
                  (octave . t)
                  (latex . t)
                )
            )

            (setq org-babel-mathematica-command "mashScript")

            ;;; Increases font size of latex preview
            (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

            (will/org-font-setup)

          '';
          extraConfig = ''
            :custom
              ;; Taken from https://gitlab.com/jabranham/emacs/-/blob/master/init.el
              (org-pretty-entities t "UTF8 all the things!")
              (org-M-RET-may-split-line nil "M-RET may never split a line.")
              (org-catch-invisible-edits 'show-and-error "Don't let me edit things I can't see.")
              (org-special-ctrl-a/e t "Make C-a and C-e work more like how I want:.")
              
              (org-highlight-latex-and-related '(latex entities) "set up fontlocking for latex")
              (org-startup-with-inline-images t "Show inline images.")
          '';
        };
        org-agenda = {
          enable = true;
          after = [ "org" ];
          defer = true;
          config = ''
            (setq org-agenda-start-with-log-mode t)
            (setq org-log-done 'time)
            (setq org-log-into-drawer t)

            ;; (setq org-directory "~/Documents/Org")
            ;; (setq org-agenda-files (list org-directory))
            (setq org-agenda-files
                  (mapcar 'abbreviate-file-name
                          (split-string
                           (shell-command-to-string "find ~/Documents -name \"*.org\"") "\n")))
          '';
        };
        org-capture = {
          enable = true;
          after = [ "org" ];
          config = ''
            (setq org-capture-templates
                  (doct '(("Tasks" :keys "t"
                           :file "~/Documents/Org/Tasks.org"
                           :prepend t
                           :template ("* %{todo-state} %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":Link: %a"
                                      ":END:"
                                      "%?")
                           :children (("Task" :keys "t"
                                       :headline "Inbox"
                                       :todo-state "TODO")))
                          ("School" :keys "s"
                           :file "~/Documents/School/School.org"
                           :prepend t
                           :template ("* %{todo-state} %^{Description}"
                                      ":PROPERTIES:"
                                      ":Created: %U"
                                      ":Link: %a"
                                      ":END:"
                                      "%?")
                           :children (("Task" :keys "t"
                                       :headline "Inbox"
                                       :todo-state "TODO"))))))
          '';
        };
        org-refile = {
          enable = true;
          after = [ "org" ];
          config = ''
            (setq org-refile-targets
                  '(("Archive.org" :maxlevel . 1)
                    ("Tasks.org" :maxlevel . 1)))
            (advice-add 'org-refile :after 'org-save-all-org-buffers)
          '';
        };
        org-tempo = {
          enable = true;
          package = "org";
          config = ''
            (add-to-list 'org-structure-template-alist '("sh" . "src shell"))            
            (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
            (add-to-list 'org-structure-template-alist '("ma" . "src mathematica"))
            (add-to-list 'org-structure-template-alist '("oc" . "src octave"))
            (add-to-list 'org-structure-template-alist '("la" . "src latex"))
          '';
        };
        org-contrib = {
          enable = true;
        };
        org-bullets = {
          enable = true;
          hook = [ "(org-mode . org-bullets-mode)" ];
        };
        mixed-pitch = {
          enable = false;
          hook = [ "(org-mode . mixed-pitch-mode)" ];
        };
        doct = {
          enable = true;
          defer = true;
          command = [ "doct" ];
        };
        wolfram-mode = {
          enable = true;
          config = ''
            (setq mathematica-command-line "mashScript")
            (add-to-list 'org-src-lang-modes '("mathematica" . wolfram))
          '';
        };

        # direnv integration
        direnv = {
          enable = true;
        };

        # LSP Mode
        lsp-mode = {
          enable = true;
          command = [ "lsp" "lsp-deferred" ];
          init = "(setq lsp-keymap-prefix \"C-c l\")";
          hook = [ "(haskell-mode . lsp)" ];
          config = ''
            ;(lsp-enable-which-key-integration t)
            
            (advice-add 'lsp :before #'direnv-update-environment)
            (setq lsp-modeline-code-actions-enable nil
                  lsp-lens-enable t
            )

          '';
        };
        lsp-ui = {
          enable = true;
          hook = [ "(lsp-mode . lsp-ui-mode)" ];
        };
        lsp-haskell = {
          enable = true;
        };
        
        # Company autocompletions with LSP
        company = {
          enable = true;
          ### after = [ "lsp-mode" "haskell-mode" ];
          hook = [ "(lsp . company-mode)" ];
          extraConfig = ''
            :bind (:map company-active-map
                    ("<tab>" . company-complete-selection))
                  (:map lsp-mode-map
                    ("<tab>" . company-indent-or-complete-common))
            :custom
            (company-minimum-prefix-length 1)
            (company-idle-delay 0.0)
          '';
        };
        company-box = {
          enable = true;
          hook = [ "(company-mode . company-box-mode)" ];
        };

        #Term mode
        term = {
          enable = true;
        };
        ############################ NIX MARKS AS BROKEN
        # eterm-256color = {
        #   enable = true;
        #   hook = [ "(term-mode . eterm-256color-mode)" ];
        # };
        vterm = {
          enable = true;
          command = [ "vterm" ];
          config = ''
            (setq vterm-max-scrollback 10000)
          '';
        };

        ### FILE MANAGEMENT
        # Dired
        dired = {
          enable = true;
          command = [ "dired" "dired-jump" ];
          config = ''
            (evil-collection-define-key 'normal 'dired-mode-map
              "h" 'dired-single-up-directory
              "l" 'dired-single-buffer)
          '';
          extraConfig = ''
            :custom ((dired-listing-switches "-agho --group-directories-first"))
          '';
        };
        dired-single = {
          enable = true;
          after = [ "dired" ];
        };
        all-the-icons-dired = {
          enable = true;
          hook = [ "(dired-mode . all-the-icons-dired-mode)" ];
        };
        dired-hide-dotfiles = {
          enable = true;
          hook = [ "(dired-mode . dired-hide-dotfiles-mode)" ];
          config = ''
            (evil-collection-define-key 'normal 'dired-mode-map
              "H" 'dired-hide-dotfiles-mode)
          '';
        };

        # pdf-tools
        pdf-tools = {
          enable = true;
          defer = 7;
          extraConfig = ''
            :custom
            (TeX-view-program-selection '((output-pdf "pdf-tools")) "Use pdf-tools to display pdfs from latex runs.")
            (TeX-view-program-list '(("pdf-tools" ("TeX-pdf-tools-sync-view") nil)))
            :magic ("%PDF" . pdf-view-mode)
          '';
          config = ''
            ;;(pdf-tools-install t)
          '';
        };
        # saveplace-pdf-view
        saveplace-pdf-view = {
          enable = true;
          config = ''
            (save-place-mode 1)
          '';
        };

        # haskell using haskell-mode
        haskell-mode = {
          enable = true;
          
        };
        haskell-doc = {
          enable = true;
        };

        # ispell
        ispell = {
          enable = true;
          extraConfig = ''
            (setq ispell-program-name "aspell")
          '';                      
        };
          
        exwm = {
          enable = true;
          config = ''
            ;; set default number of workspaces
            (setq exwm-workspace-number 5)

            ;; When window class updates, use to set buffer name
            (add-hook 'exwm-update-class-hook #'will/exwm-update-class)

            (require 'exwm-systemtray)
            (exwm-systemtray-enable)

            ;; keybinds to always go to emacs
            (setq exwm-input-prefix-keys
              '(?\C-x
                ?\C-u
                ?\C-h
                ?\M-x
                ?\M-`
                ?\M-&
                ?\M-:
                ?\C-\M-j ;; Buffer list
                ?\C-\ )) ;; Ctrl+Space

            ;; Ctrl+Q enables next key to be sent directly
            (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

            ;; Set up global key binds
            (setq exwm-input-global-keys
              `(
                ;; Reset to line mode (C-c C-k switches to char mode)
                ([?\C-r] . exwm-reset)

                ;; Move between windows
                ([s-left] . windmove-left)
                ([s-right] . windmove-right)
                ([s-up] . windmove-up)
                ([s-down] . windmove-down)

                ;; Launch applications via shell command
                ([?\s-&] . (lambda (command)
                             (interactive (list (read-shell-command "$ ")))
                             (start-process-shell-command command nil command)))

                ;; Switch workspace
                ([?\s-w] . exwm-workspace-switch)
                ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

                ;; 's-N': switch to a certain workspace with super number
                ,@(mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (exwm-workspace-switch-create ,i))))
                          (number-sequence 0 9))))

            (exwm-enable)
          '';
        };


      };

    };
  };
}
