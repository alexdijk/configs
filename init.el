;; 
;; init.el - axd
;;
;; starting with straight.el
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(if (display-graphic-p)
   (progn
     (setq initial-frame-alist
           '(
             (width . 120) ; chars
             (height . 50) ; lines       
             (left . 50)
             (top . 50)))
     (setq default-frame-alist
           '(
             (width . 120)
             (height . 50)
             (left . 50)
             (top . 50))))
 (progn
   (setq initial-frame-alist '( (tool-bar-lines . 0)))
   (setq default-frame-alist '( (tool-bar-lines . 0)))))

(if (window-system)
    (set-frame-font "Roboto Mono Medium" nil t))
;;    (set-frame-font "Fira Mono" nil t))
;;    (set-frame-font "Source Code Pro 10"))

(column-number-mode 1)
(line-number-mode 1)
;;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(load-theme 'tango-dark)
(transient-mark-mode t)

;; to diminish the statusline clutter
;; (use-package diminish)

;; smex for smarter M-x handling
(use-package smex
  :bind
  ("M-x" . smex)
  ("M-X" . smex-major-mode-commands))

;; setting up ido-mode
(use-package ido)
(ido-mode 1)
(ido-everywhere 1)

(use-package ido-completing-read+)
(ido-ubiquitous-mode 1)

(use-package ido-yes-or-no)
(ido-yes-or-no-mode t)

;; setting up Helm
;; (use-package helm
;;   :straight t)

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
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5
	which-key-popup-type 'side-window
	which-key-side-window-location 'right))

;; set the backup directory to avoid clutter
;;(setq save-place-file (concat user-emacs-directory "places")
;;      backup-directory-alist '(("." . ,(concat user-emacs-directory
;;					       "backups"))))
(let ((backup-dir (concat user-emacs-directory "/.backup")))
  (unless (file-directory-p backup-dir)
    (mkdir backup-dir t))
  (setq backup-directory-alist (cons (cons "." backup-dir) nil)))

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 5)

;; give us scratch
(setq initial-buffer-choice t
      inhibit-startup-screen t)
(show-paren-mode t)

;; org mode
(straight-use-package 'org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(use-package org-bullets
  :hook
  (org-mode . org-bullets-mode))

;; org-roam mode
(use-package org-roam
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory (file-truename "/home/alexdijk/Dropbox/Org/Zettelkasten/"))
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ) 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
