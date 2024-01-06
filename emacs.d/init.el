;; init.el - axd
;;
;; initialization starts with early-init.el

;; starting with straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)

(column-number-mode 1)
(line-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-scroll-bar-mode 'right)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
	coding-system-for-read 'utf-8
	coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package emacs
  :init
  (if (window-system)
       (set-face-attribute 'default nil
       :font "Fira Mono"
       :height 100))
  (global-visual-line-mode))

;;    (set-frame-font "Roboto Mono Medium" nil t))
;; some other standard options
;;    (set-frame-font "Fira Mono" nil t))
;;    (set-frame-font "Source Code Pro 10"))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 5)

;; ;; set the backup directory
(let ((backup-dir (concat (getenv "HOME") "/.local/share/emacs/backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))
  (setq backup-directory-alist (cons (cons backup-dir nil) nil)))

;; ;; settings need before loading the theme
;; (setq zenburn-use-variable-pitch t)
;; (setq zenburn-scale-org-headlines t)
;; (setq zenburn-scale-outline-headlines t)

;; ;; loading Zenburn theme
;; (use-package zenburn-theme
;;   :straight t)
;; (load-theme 'zenburn t)
;; (transient-mark-mode t)

(add-hook 'prog-mode-hook
	  'display-line-numbers-mode)
	
(winner-mode t)

;; setting up smart modeline
(use-package smart-mode-line
  :config
  (setq sml/name-width 40
	sml/mode-width 'full
	sml/no-confirm-load-theme t
	sml/theme 'dark)
  (sml/setup))

;; starting which-key to find keys
(use-package which-key
  :straight t
  :init
  (setq which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'bottom)
  :config
  (which-key-mode t))

;; org mode
(use-package org
  :straight t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(global-prettify-symbols-mode t)
(setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)

(use-package org-superstar
  :straight t
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
			     (org-superstar-mode t))))

;; NEEDS more research
;; ;; macro for the euro sign: "C-x 8 RET #x20AC RET""
;; (fset 'euro
;;       (lambda (&optional arg) "Keyboard macro." (interactive "p")
;; 	(kmacro-exec-ring-item
;; 	 (quote ([24 56 return 35 120 50 48 65 67 return] 0 "%d")) arg)))

;; (fset 'euro2 (kbd "C-x 8 RET 20AC RET"))

;; (defun euro3 ()
;;   "insert euro sign"
;;   (interactive "p")
;;     (insert (string (make-char 'latin-iso8859-15 164))))

(use-package emacs
  :init
  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
	  mac-command-key-is-meta t
	  mac-command-modifier 'meta
	  mac-option-modifier 'control)))
 
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

(use-package smartparens-mode
  :straight (smartparens 
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
  :straight t
  :ensure t)

(use-package vterm-toggle
  :straight t
  :ensure t
  :bind
  ("<f2>" . vterm-toggle))

;; helm
(use-package helm
  :straight t
  :init
  (helm-mode t))

(use-package helm-icons
  :straight t
  :custom
  (helm-icons-provider 'all-the-icons)
  :config
  (helm-icons-enable))

(use-package all-the-icons
  :straight t
  :config
  (all-the-icons-install-fonts 'install-without-asking))

(use-package helm-swoop
  :straight t
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
(use-package flyspell
  :straight t
  :ensure t
  :hook
  ((prog-mode . flyspell-prog-mode)
   ((org-mode markdown-mode text) . flyspell-mode))
  :config
  (custom-set-faces '(flyspell-incorrect ((t (:inverse-video t)))))
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda ()
		     (flyspell-mode 1)))))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-helm
  :straight t
  :after flyspell-correct)

(use-package markdown-mode
  :straight t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
