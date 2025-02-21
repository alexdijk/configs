;;; init.el --- the emacs init file  -*- lexical-binding: t; -*-
;;; package: --- Summary:
;;; Commentary:
;;; combiniation of settings and packages
;;; initialization starts with early-init.el
;;;
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; solve version issues
(straight-use-package 'org)
;;
;; Loading other config files
;;
(message "AD loading config-utilities.org")
(load-file "~/.config/emacs/config-utilities.el")

(message "AD loading config-org.el")
(load-file "~/.config/emacs/config-org.el")
;;
(load-file "~/.config/emacs/config-prose.el")
(message "AD loading config-prose.el")
;;
(load-file "~/.config/emacs/config-mu4e.el")
(message "AD loading config-mu4e.el")
;;
(load-file "~/.config/emacs/config-spelling.el")
(message "AD loading config-spelling.el")

;; enhancing the debug possibilities
(if debug-on-error
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; turn package warnings off
(setq byte-compile-warnings '(not obsolete))

(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; file locations
;; set the backup directory
(let ((backup-dir (concat (getenv "HOME") "/.local/share/emacs/backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))
  (setq backup-directory-alist (cons (cons "." backup-dir) nil)))
;; set the custom-file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Global key for the euro sign: "C-x 8 RET #x20AC RET""
(keymap-global-set "C-c 5" '(lambda () "euro sign" (interactive) (insert "\u20AC")))

(use-package emacs
  :straight nil
  :init
  (column-number-mode 1)
  (line-number-mode 1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode 'right)
  (recentf-mode t)
  (electric-pair-mode t)
  
  (setq inhibit-startup-message t
	inhibit-splash-screen t
	initial-scratch-message nil
	recentf-max-saved-items 2048)

  (global-set-key (kbd "M-<up>") 'windmove-up)
  (global-set-key (kbd "M-<down>") 'windmove-down)
  (global-set-key (kbd "M-<left>") 'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)

  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
	coding-system-for-read 'utf-8
	coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq-default tab-always-indent 'complete)
  (setq help-window-select t)

  ;; setting faces
  (if (display-graphic-p)
      (set-face-attribute 'default nil
			  :family "IBM Plex Mono Text"
			  :height 110
			  :weight 'regular)
    ;; normally set via customize, only ONE can be in this file.
    (custom-theme-set-faces
     'user
     '(variable-pitch
       ((t (:family "IBM Plex Sans" :weight regular :height 110))))
     '(fixed-pitch
       ((t (:family "IBM Plex Mono" :weight regular :height 110))))))

  ;; scroll one line at a time (less "jumpy" than defaults)
  ;; (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
  ;; 	mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
  ;; 	mouse-wheel-follow-mouse t                   ;; scroll window under mouse
  ;; 	scroll-step 1                                ;; keyboard scroll one line at a time
  ;; 	scroll-margin 5)
  ;; (pixel-scroll-mode)

  ;; ??
  (winner-mode t)

  ;; new window can claim all frame space
  (setq window-combination-resize nil)
  ;; (setq pop-up-windows nil)
  
  ;; Set the location:
  (setq calendar-location-name "The Hague"
	calendar-latitude 52.08
	calendar-longitude 4.31)

  ;; Set the non-standard Mac keys
  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
	  mac-command-key-is-meta t
	  mac-command-modifier 'meta
	  mac-option-modifier 'control))

  ;; linenumbers hook
  (add-hook 'prog-mode-hook
	    'display-line-numbers-mode))

;; extra themes
;; (use-package borland-blue-theme :straight t)
(use-package solarized-theme :straight t)

;; changing themes by day and night
;; (use-package theme-changer
;;   :straight t
;;   :config
;;   (message "AD configuring theme-changer, location: %s" calendar-location-name)
;;   (change-theme 'solarized-light 'solarized-dark))

;; show what the last key pressed is
;; (use-package keycast
;;   :straight t
;;   :config
;;   (keycast-header-line-mode)) 

(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode t))

(use-package casual-dired
  :straight t
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))

(use-package gnuplot
  :straight t)

;; setting up smart modeline
(use-package smart-mode-line
  :straight t
  :config
  (setq sml/name-width 20
	sml/mode-width 0
	sml/no-confirm-load-theme t
	sml/theme nil)
  (sml/setup))

;; hiding / highlighting minor-modes modeline
;; rm-blacklist - hide a few
;; rm-whitelist - hide all but a few
(use-package rich-minority
  :straight t)

;; starting which-key to find keys
(use-package which-key
  :straight t
  :init
  (setq which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'bottom)
  :config
  (which-key-mode))

(use-package casual-editkit
  :straight t
  :ensure nil
  :bind (("C-o" . casual-editkit-main-tmenu)))

;; nice writing environment
(use-package olivetti
  :straight t)

;; (use-package company
;;   :straight t
;;   :bind (:map company-active-map
;; 	      ("C-n" . company-select-next)
;; 	      ("C-p" . company-select-previous))
;;   :config
;;   (setq
;;    company-idle-delay 0.3
;;    company-backends '((company-capf
;; 		       company-elisp
;; 		       company-keywords
;; 		       company-dabbrev
;; 		       company-files))
;;    company-minimum-prefix-length 3)
;;   (global-company-mode t))

(use-package exec-path-from-shell
  :straight t
  :init (exec-path-from-shell-initialize))

;; smartparens mode / proper parens checking
(use-package smartparens-mode
  :straight smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode t)
  (turn-on-smartparens-strict-mode)
  :hook
  (prog-mode text-mode org-mode))

;; vterm / better terminal
(use-package vterm  
  :straight t 
  :commands (vterm vterm-other-window)
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
              (buffer-face-mode t))))

;; (use-package vterm-toggle
;;   :straight t
;;   :after vterm
;;   :bind
;;   ("<f2>" . vterm-toggle))

;; helm
(use-package helm
  :straight t
  :init
  (helm-mode t)
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  ;; solution to tab completion under helm
  (define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") #'helm-select-action)
  (define-key special-event-map [config-changed-event] 'ignore))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))

(use-package helm-icons
  :straight t
  :after helm
  :config
  (setq helm-icons-provider 'all-the-icons)
  (helm-icons-enable))

(use-package helm-swoop
  :straight t
  :after helm
  :config
  (setq helm-swoop-speed-or-color nil)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-pre-input-function
	(lambda () ""))
  (global-set-key (kbd "M-i") 'helm-swoop))

(use-package magit
  :straight t
  :after transient)

(use-package transient
  :straight t)

;; Common Lisp
;;(load (expand-file-name "~/.local/share/slime-helper.el"))
;;(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Python mode and config
;; Open python files in tree-sitter mode.
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package python-black
  :straight t
  :commands (python-black-buffer)
  :config
  (setq python-black-command "black")
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package jsonrpc
  :straight t)

(use-package eglot
  :straight t
  :bind (:map eglot-mode-map
	      ("C-c C-f" . eglot-format-buffer))
  :hook
  ((python-ts-mode . eglot-ensure)
   ;; (python-ts-mode . flyspell-prog-mode)
   (python-ts-mode . superword-mode)
   (python-ts-mode . hs-minor-mode)
   (python-ts-mode . (lambda () (set-fill-column 88)))))

(use-package corfu
  :straight t
  :config
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes.
  ;; :hook ((prog-mode . corfu-mode)
  ;; 	 (python-mode . corfu-mode)
  ;; 	 (shell-mode . corfu-mode)
  ;; 	 (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

;; A few more useful configurations...
(use-package emacs
  :straight nil
  :init
  ;; TAB cycle if there are only few candidates
  ;; (setq completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  ;; (setq text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(provide 'init)
;;; init.el ends here
