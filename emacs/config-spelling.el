;; Spelling and writing tools
;;
;; [ai]spell configuration
(cond ((file-exists-p "/usr/bin/aspell")
       (setq ispell-program-name "aspell")
       (setq ispell-dictionary "nl")
       (setq ispell-extra-args '("--sug-mode=ultra"  "--lang=nl")))
      ((message "ERROR: aspell not found")))

;; Flyspell mode
(dolist (hook '(text-mode-hook mu4e-compose-mode-hook))
  (add-hook hook (lambda () (flyspell-mode t))))

(dolist (hook '(change-log-mode-hook log-edit-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(eval-after-load "flyspell"
  '(progn
     (custom-set-faces '(flyspell-incorrect
			 ((t (:inverse-video t)))))))

(use-package flyspell-correct
  :straight t
  :after flyspell
  :bind
  (:map flyspell-mode-map ("C-:" . flyspell-correct-next))
  (:map flyspell-mode-map ("C-;" . flyspell-correct-previous)))

(use-package flyspell-correct-helm
  :straight t
  :after flyspell-correct)

(use-package captain
  :straight t
  :config
  (add-hook 'text-mode-hook 
            (lambda ()
              (setq captain-predicate (lambda () t)))))

(straight-use-package
  '(flymake-vale :type git :host github :repo "tpeacock19/flymake-vale"))
(add-hook 'text-mode-hook #'flymake-vale-load)
(add-hook 'org-mode-hook #'flymake-vale-load)
