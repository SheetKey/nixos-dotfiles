(use-package general
  :ensure t)

(general-def
  ;; Consult
  "C-s" 'consult-line
  "C-x b" 'consult-buffer
  "M-g M-g" 'consult-goto-line
  "M-g g" 'consult-goto-line
  "M-s f" 'consult-find
  "M-s r" 'consult-ripgrep
  ;; Helpful
  [remap describe-function] 'helpful-function
  [remap describe-symbol] 'helpful-symbol
  [remap describe-command] 'helpful-command
  [remap descrive-variable] 'helpful-variable
  [remap describe-key] 'helpful-key
  ;; IBuffer
  "C-x C-b" 'ibuffer
  ;; Embark
  "C-." 'embark-act
  ;; Avy
  "M-j" 'avy-goto-char-timer)

(general-def minibuffer-local-map
  "M-h" 'backward-kill-word
  ;; Consult
  "C-r" 'consult-history)

(general-def dired-mode-map
  [remap dired-find-file] 'dired-single-buffer
  [remap dired-mouse-find-file-other-window] 'dired-single-buffer-mouse
  [remap dired-up-directory] 'dired-single-up-directory)

(general-def company-active-map
  "C-n" 'company-select-next
  "C-p" 'company-select-previous
  "M-<" 'company-select-first
  "M->" 'company-select-last
  "TAB" 'company-complete)

(require 'org-tempo)

(use-package toc-org
  :ensure t
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :ensure t
  :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(setq org-todo-keywords
      '((sequence "TODO" "TODAY" "|" "DONE")))

(setq org-directory "~/Documents/Planning"
      org-agenda-files 
      (directory-files-recursively "~/Documents/Planning/Agenda" "\\.org$"))

(use-package org-super-agenda
  :ensure t
  :init (org-super-agenda-mode))

(setq org-super-agenda-groups
      '((:name "Today"
               :time-grid t
               :todo "TODAY")))

(use-package doct
  :ensure t)

(setq org-capture-templates
      (doct '(("Tasks" :keys "t"
               :file "~/Documents/Planning/Agenda/agenda.org"
               :prepend t
               :template ("* %{todo-state} %^{Description}"
                          "%^{Schedule|SCHEDULED|DEADLINE}: %^t"
                          ":PROPERTIES:"
                          ":Created: %U"
                          ":LINK: %a"
                          ":END:"
                          "%?")
               :children (("Task" :keys "t"
                           :headline "General"
                           :todo-state "TODO")
                          ("School" :keys "s"
                           :headline "School"
                           :todo-state "TODO")
                          )))))

(setq diary-file "~/Documents/Planning/diary")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t
      TeX-parse-self t
      TeX-electric-math (cons "$" "$")
      LaTeX-electric-left-right-brace t
      TeX-electric-sub-and-superscript t
      LaTeX-math-menu-unicode t
      TeX-insert-macro-default-style 'show-optional-args)
(add-hook 'LaTeX-mod-hook 'LaTeX-math-mode)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTex t)

(require 'lilypond-mode)

(use-package LilyPond-mode
  :mode "\\.ly\\'"
  :config
  (setq LilyPond-pdf-command "zathura"))

(use-package which-key
  :ensure t
  :init (which-key-mode 1)
  :config
  (setq which-key-idle-delay 0.5))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic t)
;;   (load-theme 'doom-solarized-dark t)
;;   (doom-themes-visual-bell-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-except-project))

;; Taken from the doom-modeline FAQ to fix right side cutoff
(setq nerd-icons-scale-factor 1.1)

(use-package all-the-icons
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :config (setq vertico-cycle t))

(use-package savehist
  :init (savehist-mode)
  :config (setq history-length 25))

(use-package consult
  :ensure t)

(use-package marginalia
  :ensure t
  :after (vertico)
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(use-package helpful
  :ensure t)

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless)))

(use-package embark
  :ensure t
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package avy
  :ensure t)

(setq avy-keys '(?q ?e ?r ?y ?u ?o ?p
                    ?a ?s ?d ?f ?g ?h ?j
                    ?k ?l ?' ?x ?c ?v ?b
                    ?n ?, ?/))

(defun avy-show-dispatch-help ()  
  (let* ((len (length "avy-action-"))
         (fw (frame-width))
         (raw-strings (mapcar
                       (lambda (x)
                         (format "%2s: %-19s"
                                 (propertize
                                  (char-to-string (car x))
                                  'face 'aw-key-face)
                                 (substring (symbol-name (cdr x)) len)))
                       avy-dispatch-alist))
         (max-len (1+ (apply #'max (mapcar #'length raw-strings))))
         (strings-len (length raw-strings))
         (per-row (floor fw max-len))
         display-strings)
    (cl-loop for string in raw-strings
             for N from 1 to strings-len do
             (push (concat string " ") display-strings)
             (when (= (mod N per-row) 0) (push "\n" display-strings)))
    (message "%s" (apply #'concat (nreverse display-strings)))))

;; Kill text
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-def avy-ring 0)))
  t)

(setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
      (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)

;; Copy text
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?w avy-dispatch-alist) 'avy-action-copy
      (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line)

;; Yank text
(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

(setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
      (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)

;; Transpose/Move text
(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

(setf (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
      (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line)

;; Mark text
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(setf (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char)

;; Get Elisp Help
(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

(setf (alist-get ?H avy-dispatch-alist) 'avy-action-helpful)

;; Embark
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)

(setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark)

(use-package undo-fu
  :ensure t)

(use-package magit
  :ensure t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dired
  :commands (dired dired-jump)
  :custom (dired-listing-switches "-agho --group-directories-first"))

(use-package dired-single
  :ensure t
  :after (dired))

(use-package dired-hide-dotfiles
  :ensure t
  :hook (dired-mode . dired-hide-dotfiles-mode))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package ispell
  :config
  (setq ispell-program-name "aspell"))

(use-package flycheck
  :ensure t)

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
                  '("mgp" "mpeg" "mp3" "mp4"
                    "avi" "wmv" "wav" "mov" "flv"
                    "ogn" "ogg" "mkv"))
                 "mpv"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("pdf"))
                 "zathura"
                 '(file))
           (list (openwith-make-extension-regexp
                  '("png" "jpeg" "jpg"))
                 "nomacs"
                 '(file))))
    (openwith-mode 1)))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 2)
  (add-hook 'after-init-hook 'global-company-mode))

(use-package eglot
  :after (direnv)
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package nix-flake
  :after (nix-mode))

(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure))

(use-package zig-mode
  :ensure t
  :config
  (add-hook 'zig-mode-hook 'eglot-ensure))

(defun will/set-font-faces ()
  (message "setting fonts")
  (set-face-attribute 'default nil
		      :font "FiraCode Nerd Font Mono"
		      :height 110
		      :weight 'medium)
  (set-face-attribute 'variable-pitch nil
		      :font "TeX Gyre Schola"
		      :height 120
		      :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
		      :font "FiraCode Nerd Font Mono"
		      :height 110
		      :weight 'medium)
  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
		      :slant 'italic))

;; Adjust line spacing
(setq-default line-spacing 0.12)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(set-fringe-mode 10)

;; Line numbers and truncation
(column-number-mode t)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)

(setq visible-bell t)

(electric-indent-mode -1)

(global-hl-line-mode 1)

(blink-cursor-mode nil)

(setq 
 ;; font options for highlighting
 modus-themes-bold-constructs t
 modus-themes-italic-constructs t

 ;; org settings
 modus-themes-org-blocks 'tinted-background

 ;; headings
 modus-themes-headings 
 '((1 . (1.5))
   (2 . (1.3)))

 ;; modeline
 modus-themes-common-palette-overrides
 '((bg-mode-line-active bg-cyan-intense)
   (fg-mode-line-active fg-main)
   (border-mode-line-active blue-intense)))


;; This should be the last line of theme config: set variables first.
(load-theme 'modus-vivendi-deuteranopia t)

(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(setq doom-modeline-icon t)
		(with-selected-frame frame
		  (will/set-font-faces))))
  (will/set-font-faces))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t
      version-control t
      delete-old-version t
      kept-new-version 20
      kept-old-version t)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(global-auto-revert-mode 1)
;; Revert dired
(setq global-auto-revert-non-file-buffers t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("org" (name . "^.*org$"))
               ("programming" (or
                               (mode . haskell-mode)
                               (mode . nix-mode)))
               ("magit" (name . "magit*"))
               ("emacs" (or
                         (name . "^\\*scratch*$")
                         (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(require 'dash)

(defvar-local will/tab-out-delimiters 
    '(";" "(" ")" "[" "]" "{" "}" "|" "'" "\"" "`" "$" "<" ">"))

(defun will/get-line-from-cursor (arg)
  (interactive "P")
  (let ((not-end-of-line (buffer-substring-no-properties
                          (+ 1 (point))
                          (line-end-position))))
    (if not-end-of-line
        (buffer-substring-no-properties
         (+ 1 (point))
         (line-end-position))
      (buffer-substring-no-properties
       (point)
       (line-end-position)))))

(defun will/contains-delimiter (arg)
  (-first
   (lambda (head)
     (-contains? will/tab-out-delimiters head))
   (split-string arg "")))

(defun will/line-contains-delimeter (arg)
  (interactive "P")
  (will/contains-delimiter (will/get-line-from-cursor nil)))

(defun will/tab-out (arg)
  "Jump out of a parenthetical."
  (interactive "P")
  (let ((str (will/line-contains-delimeter nil)))
    (if str
        (search-forward str)
      (will/tab-fallback))))

(defun will/tab-fallback ()
  "Fallback behavior of `will/exit-parens`."
  (let ((fallback-behavior (will/tab-original-keybinding)))
    (if fallback-behavior
        (call-interactively fallback-behavior))))

(defun will/tab-original-keybinding ()
  "Get current keys' binding as if `will/exit-parens` didn't exist."
  ;; Copied from tab-jump-out package
  (let* ((will/tab-out-mode nil)
         (keys (this-single-command-keys)))
    (or (key-binding keys t)
        (key-binding (will/tab-out--fallback-translate-input keys) t))))

(defun will/tab-out--fallback-translate-input (keys)
  ;; Copied from tab-jump-out package
  (let ((retval [])
        (i 0))
    (while (< i (length keys))
      (let ((j i)
            (translated local-function-key-map))
        (while (and (< j (length keys))
                    translated
                    (keymapp translated))
          (setq translated (cdr (assoc (aref keys j) (remove 'keymap translated)))
                j (1+ j)))
        (setq retval (vconcat retval (cond ((symbolp translated)
                                            `[,translated])
                                           ((vectorp translated)
                                            translated)
                                           (t
                                            (substring keys i j)))))
        (setq i j)))
    retval))

(defgroup will/tab-out nil
  "Custom group for `will/tab-out-mode`."
  :group 'editing
  :prefix "will/tab-out-")

(defvar will/tab-out-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'will/tab-out)
    map)
  "Keymap for `will/tab-out`.")

(define-minor-mode will/tab-out-mode
  "A minor mode that allows you to jump out of parentheticals with tab."
  :keymap will/tab-out-mode-map
  :global t)

;; (will/tab-out-mode 1)
;; (add-hook 'minibuffer-setup-hook (lambda () (will/tab-out-mode -1)))
;; (add-hook 'minibuffer-exit-hook (lambda () (will/tab-out-mode 1)))
