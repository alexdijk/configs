;; pre start initialization

(setq gc-cons-threshold (eval-when-compile (* 128 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

(push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)

;; setting initial screen sizes
(if (display-graphic-p)
   (progn
     (setq initial-frame-alist
           '(
             (width . 120) ; chars
             (height . 60) ; lines       
             (left . 50)
             (top . 50)))
     (setq default-frame-alist
           '(
             (width . 120)
             (height . 60)
             (left . 50)
             (top . 50))))
(progn
  (setq initial-frame-alist '( (tool-bar-lines . 0)))
  (setq default-frame-alist '( (tool-bar-lines . 0)))))

(setq package-enable-at-startup nil)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

