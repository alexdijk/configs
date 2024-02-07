;; init.el - axd
;;
;; updated to packagemanagement by elpaca
;;
;; initialization starts with early-init.el
;; Elpaca configuration -*- lexical-binding: t; -*-
;;
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.
;; (elpaca example-package)

(defmacro use-feature (name &rest args)
  "Like `use-package' but accounting for asynchronous installation.
  NAME and ARGS are in `use-package'."
  (declare (indent defun))
  `(use-package ,name
     :elpaca nil
     ,@args))

;; Install use-package support
(elpaca elpaca-use-package
  (require 'elpaca-use-package)
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

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

(column-number-mode 1)
(line-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-scroll-bar-mode 'right)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

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
;; possible options: "Roboto Mono Medium", "Fira Mono" or "Source Code Pro 10"
(if (window-system)
    (set-face-attribute 'default nil
			:family "JetBrains Mono"
			:height 120
			:weight 'regular))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 5)
;; (setq mwheel-coalesce-scroll-events nil)
;; (pixel-scroll-precision-mode t)

;; set the backup directory
(let ((backup-dir (concat (getenv "HOME") "/.local/share/emacs/backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))
  (setq backup-directory-alist (cons (cons "." backup-dir) nil)))

;; loading Zenburn theme
;; settings need before loading the theme
;; (setq zenburn-use-variable-pitch t)
;; (setq zenburn-scale-org-headlines t)
;; (setq zenburn-scale-outline-headlines t)
;; ---
;; (use-package zenburn-theme
;;   :elpaca t)
;; (load-theme 'zenburn t)
;; (transient-mark-mode t)

;; extra theme
(use-package borland-blue-theme :demand t)
;; linenumbers hook
(add-hook 'prog-mode-hook
	  'display-line-numbers-mode)

(winner-mode t)

;; setting up smart modeline
(use-package smart-mode-line
  :elpaca t
  :config
  (setq sml/name-width 40
	sml/mode-width 'full
	sml/no-confirm-load-theme t
	sml/theme 'dark)
  (sml/setup))

;; starting which-key to find keys
(use-package which-key
  :elpaca t
  :init
  (setq which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'bottom)
  :config
  (which-key-mode))

;; org mode
(use-package org
  :init (require 'org)
  :config
  (global-prettify-symbols-mode t)
  (setq org-hide-leading-stars t)
  (setq org-src-fontify-natively t)
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ;; Zero Width Space / zwsp
  ("C-c M-0" . (lambda () (interactive) (insert "\u200B")))
  :hook
  (org-mode . visual-line-mode))
(elpaca-wait)

(use-package org-superstar
  :elpaca t
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
			     (org-superstar-mode t))))

;; Global key for the euro sign: "C-x 8 RET #x20AC RET""
(keymap-global-set "C-c 5" '(lambda () "euro sign" (interactive) (insert "\u20AC")))

;; Set the non-standard Mac keys
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta nil
  mac-command-key-is-meta t
  mac-command-modifier 'meta
  mac-option-modifier 'control))

;; Some LateX settings
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; smartparens mode / proper parens checking
(use-package smartparens-mode
  :elpaca (smartparens 
	      :host github
	      :repo "Fuco1/smartparens"
	      :files (:defaults))
  :ensure smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode 1)
  (turn-on-smartparens-strict-mode)
  :hook (prog-mode text-mode markdown-mode org-mode)
  :custom (sp-highlight-pair-overlay nil))

;; vterm / better terminal
(use-package vterm
  :elpaca (vterm
	   :post-build
	   (progn
	     (setq vterm-always-compile-module t)
	     (require 'vterm)
	     (with-current-buffer (get-buffer-create vterm-install-buffer-name)
	       (goto-char (point-min))
	       (while (not (eobp))
		 (message "%S"
			  (buffer-substring (line-beginning-position)
					    (line-end-position)))
		 (forward-line)))
	     (when-let ((so (expand-file-name "./vterm-module.so"))
			((file-exists-p so)))
	       (make-symbolic-link
		so (expand-file-name (file-name-nondirectory so)
				     "../../builds/vterm")
		'ok-if-already-exists))))
  :commands (vterm vterm-other-window))

(use-package vterm-toggle
   :elpaca t
   :ensure t
   :after vterm
   :bind
   ("<f2>" . vterm-toggle))

;; helm
(use-package helm
  :elpaca t
  :init
  (helm-mode t))
(elpaca-wait)

(use-package helm-icons
  :elpaca t
  :custom
  (helm-icons-provider 'all-the-icons)
  :config
  (helm-icons-enable))

(use-package all-the-icons
  :elpaca t
  :config
  (all-the-icons-install-fonts 'install-without-asking))

(use-package helm-swoop
  :elpaca t
  :custom
  (helm-swoop-speed-or-colorl nil "Give up color for speed")
  (helm-swoop-split-with-multiple-windows nil "Do not split window"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-i") 'helm-swoop)

;; solution to tab completion under helm
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)
(define-key special-event-map [config-changed-event] 'ignore)

;; [ai]spell configuration
(cond ((file-exists-p "/usr/bin/aspell")
       (setq ispell-program-name "aspell")
       (setq ispell-dictionary "nl")
       (setq ispell-extra-args '("--sug-mode=ultra"  "--lang=nl")))
      ((message "ERROR: aspell not found")))

;; Flyspell mode
(use-feature flyspell
  :commands (flyspell-mode flyspell-prog-mode)
  :ensure t
  :config
  (custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
  (setq hook '(text-mode-hook org-mode mu4e-compose-mode git-commit-mode markdown-mode))
  :hook
  ('hook . flyspell-mode))

(use-package flyspell-correct
  :elpaca t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :elpaca t
  :after flyspell-correct)

(use-package markdown-mode
  :elpaca t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Common Lisp
(load (expand-file-name "~/.local/share/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; load email configuration
(load-file "~/.emacs.d/mu4e-conf.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("296da7c17c698e963c79b985c6822db0b627f51474c161d82853d2cb1b90afb0" "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" default))
 '(package-selected-packages '(eglot slime ef-themes ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flyspell-incorrect ((t (:inverse-video t)))))
