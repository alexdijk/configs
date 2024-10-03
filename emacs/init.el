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

  (setq inhibit-startup-message t
	inhibit-splash-screen t
	initial-scratch-message nil
	recentf-max-saved-items 2000)

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

  ;; setting faces

  (if (display-graphic-p)
      (set-face-attribute 'default nil
			  :family "IBM Plex Mono Text"
			  :height 105
			  :weight 'regular)
    ;; normally set via customize, only ONE can be in this file.
    (custom-theme-set-faces
     'user
     '(variable-pitch
       ((t (:family "IBM Plex Sans" :weight regular :height 105))))
     '(fixed-pitch
       ((t (:family "IBM Plex Mono" :weight regular :height 105))))))


  ;; scroll one line at a time (less "jumpy" than defaults)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
	mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
	mouse-wheel-follow-mouse t                   ;; scroll window under mouse
	scroll-step 1                                ;; keyboard scroll one line at a time
	scroll-margin 5)
  (pixel-scroll-mode)

  (winner-mode t)

  ;; new window can claim all frame space
  (setq window-combination-resize nil)
  (setq pop-up-windows nil)
  
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

  ;; Little vi hack: match parenthesis with '%'
  (global-set-key "%" 'match-paren)
  (defun match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
          ((looking-at "\\s)") (forward-char 1) (backward-list 1))
          (t (self-insert-command (or arg 1)))))

  ;; linenumbers hook
  (add-hook 'prog-mode-hook
	    'display-line-numbers-mode))

;; extra themes
;; (use-package borland-blue-theme :straight t)
(use-package solarized-theme :straight t)

;; changing themes by day and night
(use-package theme-changer
  :straight t
  :config
  (message "AD configuring theme-changer, location: %s" calendar-location-name)
  (change-theme 'solarized-light 'solarized-dark))

;; show what the last key pressed is
;; (use-package keycast
;;   :straight t
;;   :config
;;   (keycast-header-line-mode)) 

(use-package casual-dired
  :straight t
  :bind (:map dired-mode-map ("C-o" . #'casual-dired-tmenu)))

;; mu4e email configuration
(message "AD loading config-mu4e.el")
(load-file "home/alexdijk/.config/emacs/config-mu4e.el")
;; nice columns in mu4e
(use-package mu4e-column-faces
  :straight t
  :after mu4e
  :config (mu4e-column-faces-mode))

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

;; nice writing environment
(use-package olivetti
  :straight t)

(use-package vulpea
  :straight t
  ;; hook into org-roam-db-autosync-mode you wish to enable
  ;; persistence of meta values (see respective section in README to
  ;; find out what meta means)
  :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

;; org mode
(use-package org
  :init (require 'org)
  :config
  (define-key org-mode-map (kbd "<M-up>") nil)
  (define-key org-mode-map (kbd "<M-down>") nil)
  (variable-pitch-mode t)
  (global-prettify-symbols-mode t)
  (add-to-list 'font-lock-extra-managed-props 'display)
  (font-lock-add-keywords 'org-mode
			  `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
			     (1 `(face nil display
				       (space :align-to
					      (- right ,(length
							 (match-string 2)) 3)))
				prepend))) t)
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 ()
				  (compose-region (match-beginning 1)
						  (match-end 1) "•"))))))
  
  (let* ((variable-tuple
	  (cond ((x-list-fonts "IBM Plex Sans") '(:font "IBM Plex Sans"))
		(nil (warn "Cannot find IBM Plex"))))
	 (base-font-color (face-foreground 'default nil 'default))
	 (headline '(:inherit default :weight bold :foreground ,base-font-color))))

  (custom-theme-set-faces
   'user
   '(org-tag ((t (:inherit
		  (shadow fixed-pitch) :weight bold :height 1.0))))
   '(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
   '(org-level-3 ((t (,@headline ,@variable-tuple
				 :height 1.15
				 :foreground "cornflower blue"))))
   '(org-level-2 ((t (,@headline ,@variable-tuple
				 :height 1.2
				 :foreground "dodger blue"))))
   '(org-level-1 ((t (,@headline ,@variable-tuple
				 :height 1.25
				 :weight bold
				 :foreground "royal blue"))))
   '(org-document-title
     ((t (,@headline ,@variable-tuple :height 1.0 :underline nil)))))
  
  (setq  org-agenda-files '("~/Work/Aantekeningen/"
			    "~/Work/RoamNotes/"))
 
  (setq org-startup-indented t
	org-hide-leading-stars t
	org-hide-emphasis-markers t
	org-src-fontify-natively t
	org-hide-block-startup t
	org-startup-folded "fold"
	org-ellipsis " ⤵"
	org-pretty-entities t)
  (setq org-tag-alist '(("Project" . ?p) ("Idea" . ?i) ("Action" . ?a) ("Private" . ?h)))
  (setq org-todo-keywords '((sequence "TODO" "WAIT" "VERIFY" "DONE")))
  (message "AD loading config-org.el")
  (load-file "/home/alexdijk/.config/emacs/config-org.el")
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ;; Zero Width Space / zwsp
  ("C-c M-0" . (lambda () (interactive) (insert "\u200B")))
  :hook
  (org-mode . visual-line-mode))

;; hide bold/underlined/slanted
(use-package org-appear
  :straight t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis   t
        org-hide-emphasis-markers t
        org-appear-autolinks      nil
        org-appear-autoentities   t
        org-appear-autosubmarkers t)
  (run-at-time nil nil #'org-appear--set-elements))

;; ;; Modes can also require some fixed-pitch fonts for some elements
;; ;; of the buffer, such as code blocks with org-mode mixed-pitch comes
;; ;; to the rescue!
(use-package mixed-pitch
  :straight t
  :requires org
  :hook
  (org-mode . mixed-pitch-mode)
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (mixed-pitch-mode -1))))

(use-package org-bullets
  :straight t
  :config
  (add-hook 'org-mode-hook (lambda ()
			     (org-bullets-mode t))))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ;; using org mode with many cross references.
	 ("C-c j" . org-previous-link)
	 ("C-c k" . org-next-link)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (setq org-roam-directory "/home/alexdijk/Work/RoamNotes")
  (setq org-roam-completion-everywhere t)
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-db-autosync-mode t)
  (org-roam-setup))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; Bind this function to C-c n I
(keymap-global-set "C-c n I" 'org-roam-node-insert-immediate)

(use-package org-roam-ui
  :straight t
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start t))

(use-package company
  :straight t
  :bind (:map company-active-map
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous))
  :config
  (setq
   company-idle-delay 0.3
   company-backends '((company-capf company-elisp))
   company-minimum-prefix-length 3)
  (global-company-mode t))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12(vulpea-agenda-category 12)%?-12t% s")
        (todo . " %i %-12(vulpea-agenda-category 12) ")
        (tags . " %i %-12(vulpea-agenda-category 12) ")
        (search . " %i %-12(vulpea-agenda-category 12) ")))

(defun vulpea-agenda-category (&optional len)
  "Get category of item at point for agenda.
Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

Usage example:
  (setq org-agenda-prefix-format
        '((agenda . \" %(vulpea-agenda-category) %?-12t %12s\")))
Refer to `org-agenda-prefix-format' for more information."
  
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
	 (result
	  (or (if (and
		   title
		   (string-equal category file-name))
		  title
		category)
              "")))
    (if (numberp len)
	(s-truncate len (s-pad-right len " " result))
      result)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

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
;;(use-package vterm  
;;  :straight t 
;;  :commands (vterm vterm-other-window)
;;  :config
;;  (add-hook 'vterm-mode-hook
;;            (lambda ()
;;              (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
;;              (buffer-face-mode t))))
;;
;;(use-package vterm-toggle
;;  :straight t
;;  :after vterm
;;  :bind
;;  ("<f2>" . vterm-toggle))

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

;; [ai]spell configuration
(cond ((file-exists-p "/usr/bin/aspell")
       (setq ispell-program-name "aspell")
       (setq ispell-dictionary "nl")
       (setq ispell-extra-args '("--sug-mode=ultra"  "--lang=nl")))
      ((message "ERROR: aspell not found")))

;; Helm additions to consider:
;; https://github.com/emacs-helm/helm-mu
;; helm-slime

;; Flyspell mode
;;(use-package flyspell
;;  :straight nil
;;  :config
;;  (custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
;;  (define-key flyspell-mode-map (kbd "C-,") nil)
;;  (define-key flyspell-mode-map (kbd "C-.") nil)
;;  (define-key flyspell-mode-map (kbd "C-;") #'flyspell-correct-wrapper)
;;  :hook
;;  (text-mode-hook mu4e-compose-mode))
;;
;;(use-package flyspell-correct
;;  :straight nil
;;  :after flyspell
;;  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
;;  :commands (flyspell-correct-at-point
;;	     flyspell-correct-wrapper))
;;
;;(use-package flyspell-correct-helm
;;  :straight t
;;  :after flyspell-correct)

;; (use-package flycheck
;;   :straight t
;;   :commands (flycheck-mode)
;;   :custom (flycheck-emacs-lisp-load-path
;; 	   'inherit "necessary with alternatives to package.el"))

;; (use-package flycheck-package
;;   :straight t
;;   :after (flychceck)
;;   :config (flycheck-package-setup)
;;   (add-to-list 'display-buffer-alist
;;                '("\\*Flycheck errors\\*"
;; 		 display-buffer-below-selected (window-height . 0.15))))

(use-package magit
  :straight t
  :after transient)

(use-package transient
  :straight t)

;; ;; Yasnippet snippet / template system
;; (use-package yasnippet
;;   :straight t
;;   :hook ((text-mode
;; 	  prog-mode
;; 	  conf-mode
;; 	  snippet-mode) . yas-minor-mode-on)
;;   :init
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"
;; 			   "~/.emacs.d/elpaca/builds/yasnippet-snippets/snippets")))

;; (use-package yasnippet-snippets
;;   :straight t
;;   :after yasnippet)

;; (use-package yatemplate
;;   :straight t
;;   :after yasnippet)

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
  ;; Optional customizations
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
  :hook ((prog-mode . corfu-mode)
	 (shell-mode . corfu-mode)
	 (eshell-mode . corfu-mode))

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
