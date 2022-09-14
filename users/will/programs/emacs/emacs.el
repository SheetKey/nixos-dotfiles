;;(setq use-package-always-ensure t)

;; UI Changes
(scroll-bar-mode -1)                ; Disable visible scrollbar
(tool-bar-mode -1)                  ; Disable the toolbar
(tooltip-mode -1)                   ; Disable tooltips
(set-fringe-mode 10)                ; Give breathing room
(menu-bar-mode -1)                  ; Disable menu bar

;; Backup
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
                vterm-mode-hook
                pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set up visual bell
(setq visible-bell t)

;; Font
(defun will/set-font-faces ()
  (message "setting fonts")
  (set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 110)
  (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font Mono")
  (set-face-attribute 'variable-pitch nil :font "TeX Gyre Schola")
)

;; Do stuff when using daemon
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
                (with-selected-frame frame
                  (will/set-font-faces))))
    (will/set-font-faces))

    
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

(use-package general
  :ensure t
)

;; Gloabl keys
(general-def
 "<escape>" 'keyboard-escape-quit
 ;; Consult
 "C-S" 'consult-line
 "C-x b" 'consult-buffer
 "M-g M-g" 'consult-goto-line
 "M-g g" 'consult-goto-line
 "M-s f" 'consult-find
 "M-s r" 'consult-ripgrep
 ;; Helpful
 [remap describe-function] 'helpful-function
 [remap describe-symbol] 'helpful-symbol
 [remap describe-command] 'helpful-command
 [remap describe-variable] 'helpful-variable
 [remap describe-key] 'helpful-key
)
;; Evil global keys
(general-def 'motion
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
)
;; Vertico keys
(general-def vertico-map
	     "C-j" 'vertico-next
	     "C-k" 'vertico-previous
	     "C-f" 'vertico-exit
)
;; Minibuffer keys
(general-def minibuffer-local-map
	     "M-h" 'backward-kill-word
	     ;; Consult
	     "C-r" 'consult-history
)
;; Dired keys
(general-def 'normal dired-mode-map
  "h" 'dired-single-up-directory
  "l" 'dired-single-buffer
  "H" 'dired-hide-dotfiles-mode
)
;; Dired single
(general-def dired-mode-map
  [remap dired-find-file] 'dired-single-buffer
  [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse
  [remap dired-up-directory] 'dired-single-up-directory
)

(use-package doom-themes
  :ensure t
  :config
    ;; Global settings
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (load-theme 'doom-one t)

    (doom-themes-visual-bell-config)
)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
    (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
)

;; Taken from doom modeline FAQ
(eval-after-load "doom-modeline"
  (doom-modeline-def-modeline 'main
      '(bar matches buffer-info remote-host buffer-position parrot selection-info)
      '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs "  "))) 

(use-package all-the-icons
  :ensure t
)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
    (setq which-key-idle-delay 0.5)
)

;; NEEDS KEYBINDS
(use-package vertico
  :ensure t
  :init (vertico-mode)
  :config
    (setq vertico-cycle t)
)

(use-package savehist
  :init (savehist-mode)
  :config
    (setq history-length 25)
)

(use-package orderless
  :ensure t
  :custom
    (completion-styles '(orderless))
)

;; NEEDS KEYBINDS
(use-package marginalia
  :ensure t
  :after (vertico)
  :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

;; NEEDS KEYBINDS
(use-package consult
  :ensure t
)

;; NEEDS KEYBINDS
(use-package helpful
  :ensure t
)

;; NEEDS KEYBINDS
(use-package evil
  :ensure t
  :init
    (setq evil-want-integration t
          evil-want-keybinding nil
          evil-want-C-u-scroll nil
          evil-want-C-i-jump nil
          evil-undo-system 'undo-fu
    )
  :config
    (evil-mode 1)
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal)
)

;; NEEDS KEYBINDS
(use-package evil-collection
  :ensure t
  :after (evil)
  :config (evil-collection-init)
)

(use-package undo-fu
  :ensure t
)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
)

(use-package nix-flake
  :after (nix-mode)
)

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom 
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
)

(use-package ssh-agency
  :ensure t
)

(use-package direnv
  :ensure t
  :config (direnv-mode)
)

;; NEEDS KEYBINDS
(use-package dired
  :commands (dired dired-jump)
  :custom (dired-listing-switches "-agho --group-directories-first")
)

;; NEEDS KEYBINDS
(use-package dired-single
  :ensure t
  :after (dired)
)

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode)
)

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
)  

(use-package ispell
  :config
    (setq ispell-program-name "aspell")
)

(use-package openwith
  :ensure t
  :config
    (when (require 'openwith nil 'noerror)
      (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
            '("doc" "xls" "ppt" "odt" "ods" "odg" "odp"))
            "libreoffice"
            '(file))
          (list (openwith-make-extension-regexp
            '("mpg" "mpeg" "mp3" "mp4"
              "avi" "wmv" "wav" "mov" "flv"
              "ogm" "ogg" "mkv"))
            "mpv"
            '(file))
          (list (openwith-make-extension-regexp
            '("pdf" "png" "jpeg"))
            "zathura"
            '(file))
        )
      )
      (openwith-mode 1)
    )
)

(use-package haskell-mode
  :ensure t
)
  
