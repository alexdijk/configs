;; 
;; init.el - axd
;;

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

;; ;; setting initial screen sizes
;; (if (display-graphic-p)
;;    (progn
;;      (setq initial-frame-alist
;;            '(
;;              (width . 120) ; chars
;;              (height . 50) ; lines       
;;              (left . 50)
;;              (top . 50)))
;;      (setq default-frame-alist
;;            '(
;;              (width . 120)
;;              (height . 50)
;;              (left . 50)
;;              (top . 50))))
;; (progn
;;   (setq initial-frame-alist '( (tool-bar-lines . 0)))
;;   (setq default-frame-alist '( (tool-bar-lines . 0)))))

(if (window-system)
    (set-frame-font "Roboto Mono Medium" nil t))
;; some other standard options
;;    (set-frame-font "Fira Mono" nil t))
;;    (set-frame-font "Source Code Pro 10"))

(column-number-mode 1)
(line-number-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-scroll-bar-mode 'right)

(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 5)

(let ((backup-dir(concat user-emacs-directory "./backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))
  (setq backup-directory-alist (cons(cons "." backup-dir) nil)))

;; loading Zenburn theme
;; settings need before loading the theme
(setq zenburn-use-variable-pitch t)
(setq zenburn-scale-org-headlines t)
(setq zenburn-scale-outline-headlines t)

(use-package zenburn-theme
  :straight t)
(load-theme 'zenburn t)
(transient-mark-mode t)

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
  ;;  :diminish which-key-mode
  :straight t
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'bottom))

;; org mode
(use-package org
  :straight t)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package org-bullets
  :straight t
  :hook
  (org-mode . org-bullets-mode))

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

;; specific Mac options, Cmd becomes Meta, Option becomes Ctrl 
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'control)

;; [ai]spell configuration
(setq-default ispell-program-name "aspell")

(cond
((executable-find "aspell")
 (setq ispell-program-name "aspell")
 (setq ispell-extra-args '("--sug-mode=ultra"  "--lang=nl")))
(message "aspell not found"))
 
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)

;; smartparens
(use-package smartparens
  :straight t)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(show-smartparens-global-mode 1)
;;(add-hook 'rust-mode-hook #'smartparens-mode)
;;(add-hook 'minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;;(show-paren-mode t)

(use-package projectile
  :straight t
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-command-map)
	      ("C-c p" . projectile-command-map)))

;; vterm / better terminal
(use-package vterm
  :straight t
  :ensure t)

;; config vterm
(global-set-key [f2] 'vterm-toggle)
(global-set-key [C-f2] 'vterm-toggle-cd)
(define-key vterm-mode-map [(control return)] #'vterm-toggle-insert-cd)
(define-key vterm-mode-map (kbd "s-n") 'vterm-toggle-forward)
(define-key vterm-mode-map (kbd "s-p") 'vterm-toggle-backward)

;; helm
(use-package helm
  :straight t)
;;(require 'helm-config)

(helm-autoresize-mode t)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-i") 'helm-swoop)

;; solution to tab completion under helm
(define-key helm-map (kbd "TAB") #'helm-execute-persistent-action)
(define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") #'helm-select-action)

(define-key special-event-map [config-changed-event] 'ignore)

(use-package flyspell-correct-helm
  :straight t)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-wrapper)

(use-package markdown-mode
  :straight t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; ;; give us scratch
;; (setq initial-buffer-choice t
;;       inhibit-startup-screen t)

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
