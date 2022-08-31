(setq use-package-always-ensure t)

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


(use-package doom-themes
  :config
    ;; Global settings
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (load-theme 'doom-tokyo-night t)

    (doom-themes-visual-bell-config)
)

(use-package doom-modeline
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
)

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode
)

;; NEEDS KEYBINDS
(use-package which-key
  :init (which-key-mode)
  :config
    (setq which-key-idle-delay 0.5)
)

;; NEEDS KEYBINDS
(use-package vertico
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
  :config
    (setq completion-styles '(orderless))
)

;; NEEDS KEYBINDS
(use-package marginalia
  :after (vertico)
  :custom
    (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
)

;; NEEDS KEYBINDS
(use-package consult
)

;; NEEDS KEYBINDS
(use-package helpful
)

;; NEEDS KEYBINDS
(use-package evil
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
  :after (evil)
  :config (evil-collection-init)
)

(use-package undo-fu
)

(use-package nix-mode
  :mode "\\.nix\\'"
)

(use-package nix-flake
  :after (nix-mode)
)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom 
    (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
)

(use-package ssh-agency
)

(use-package direnv
  :config (direnv-mode)
)

;; NEEDS KEYBINDS
(use-package dired
  :commands (dired dired-jump)
)

;; NEEDS KEYBINDS
(use-package dired-single
  :after (dired)
)

(use-package all-the-icons-dired
  :hook (dired-mode)
)  

(use-package ispell
  :config
    (setq ispell-program-name "aspell")
)

(use-package openwith
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

