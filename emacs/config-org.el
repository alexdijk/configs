;; org mode specific configuration
;;
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
	  (if (display-graphic-p)
	      (cond ((x-list-fonts "IBM Plex Sans") '(:font "IBM Plex Sans"))
		    (nil (warn "Cannot find IBM Plex")))))
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
  
  (setq  org-agenda-files '("~/ORG/Aantekeningen/"
			    "~/ORG/RoamNotes/"
			    "~/ORG/Docs/")
	 org-directory "/home/alexdijk/ORG"
	 org-roam-directory "/home/alexdijk/ORG/RoamNotes")

  (setq org-startup-indented t
	org-hide-leading-stars t
	org-hide-emphasis-markers t
	org-src-fontify-natively t
	org-hide-block-startup t
	org-startup-folded "fold"
	org-ellipsis " ⤵"
	org-pretty-entities t
	org-property-format "%-24s %s")

  (setq org-tag-alist
	'(("planning" . ?j)
	  ("action" . ?a)	  
	  ("private" . ?h)
	  ("work" . ?p)
	  ("idea" . ?i)
	  ("writing" . ?w)
	  ("email" . ?m)
	  ("calls" . ?c)
	  ("urgent" . ?u)
	  ("important" . ?x)
	  ("meeting" . ?b)
	  ("eu" . ?e)
	  ("nl" . ?n)))
  
  (setq org-todo-keywords '((sequence "TODO" "PLAN" "WAIT" "DONE")))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)

  ;; Zero Width Space / zwsp
  ("C-c M-0" . (lambda () (interactive) (insert "\u200B")))
  :hook
  (org-mode . visual-line-mode)
  (org-agenda-finalize . hl-line-mode))

(add-to-list 'org-link-frame-setup '(file. find-file))

(defun org-force-open-current-window ()
  (interactive)
  (let ((org-link-frame-setup (quote
                               ((vm . vm-visit-folder)
                                (vm-imap . vm-visit-imap-folder)
                                (gnus . gnus)
                                (file . find-file)
                                (wl . wl)))
                              ))
    (org-open-at-point)))

;; Depending on universal argument try opening link
(defun org-open-maybe (&optional arg)
  (interactive "P")
  (if arg
      (org-open-at-point)
    (org-force-open-current-window)))

;; Redefine file opening without clobbering universal argumnet
(define-key org-mode-map "\C-c\C-o" 'org-open-maybe)

;; TODO does funtion org-aligh-all-tags still exist??
(add-hook 'focus-in-hook 
	  (lambda () (progn 
		  (setq org-tags-column
			(- 5 (window-body-width)))) (org-align-all-tags)))

(add-hook 'focus-out-hook 
	  (lambda () (progn 
		  (setq org-tags-column
			(- 5 (window-body-width)))) (org-align-all-tags)))

;; ;; Originally from here: https://stackoverflow.com/a/59001859/2178312
;; (defun gopar/get-schedule-or-deadline-if-available ()
;;   "Return an calendar emoji if current item doens't have aschedule or deadline.Otherwise return an empty string."
;;   (let ((scheduled (org-get-scheduled-time (point)))
;;         (deadline (org-get-deadline-time (point))))
;;     (if (not (or scheduled deadline))
;;         (format " ")
;;       "   ")))

(customize-set-variable 'org-agenda-remove-tags t)
(setq org-agenda-prefix-format
      '((agenda . "  %-12T")
	(todo . "%-20:c%t")
;;	(todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
	(tags . "%-12s")
	(search . "%-12:c"))
      org-agenda-todo-keyword-format "%-1s"
      org-agenda-tags-column 0)

(setq org-agenda-custom-commands
      '(("c" "Simple agenda view" alltodo "")))

(defun org-agenda-open-hook ()
  (olivetti-mode))
(add-hook 'org-agenda-mode-hook 'org-agenda-open-hook)

(use-package org-super-agenda
  :straight t)

;; (let ((org-agenda-span 'day)
;;       (org-super-agenda-groups
;;        '((:name "Time grid items in all-uppercase with RosyBrown1 foreground"
;;                 :time-grid t
;;                 :transformer (--> it
;;                                   (upcase it)
;;                                   (propertize it 'face '(:foreground "RosyBrown1"))))
;;          (:name "Priority >= C items underlined, on black background"
;;                 :face (:background "black" :underline t)
;;                 :not (:priority>= "C")
;;                 :order 100))))
;;   (org-agenda nil "a"))

(use-package org-contrib
  :straight t)
(use-package org-secretary
  :straight t)

(use-package org-fancy-priorities
  :straight t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-priority-highest 0
	org-priority-default 2
	org-priority-lowest 4)
  (setq org-fancy-priorities-list
	'((?0 . "P0")
          (?1 . "P1")
          (?2 . "P2")
          (?3 . "P3")
          (?4 . "P4"))
	org-priority-faces
	'((?0 :foreground "DarkRed" :background "LightPink")
          (?1 :foreground "DarkOrange4" :background "LightGoldenrod")
          (?2 :foreground "gray20" :background "gray")
          (?3 :foreground "gray20" :background "gray")
          (?4 :foreground "gray20" :background "gray"))))

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
  ;; (text-mode . mixed-pitch-mode)
  (org-agenda-mode . (lambda () (mixed-pitch-mode -1))))

(use-package org-superstar
  :straight t
  :hook
  (org-mode . org-superstar-mode))

;; (use-package org-bullets
;;   :straight t
;;   :hook
;;   (org-mode . (lambda () (org-bullets-mode t))))

;; (use-package org-modern
;;   :straight t)
;; (with-eval-after-load 'org (global-org-modern-mode))

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


(setq org-latex-compiler "xelatex")
(setq org-latex-pdf-process
      (list (concat "latexmk -"
                    org-latex-compiler 
                    " -recorder -synctex=1 -bibtex-cond %b")))
(setq org-latex-listings t)
(setq org-latex-default-packages-alist
      '(("" "graphicx" t)
        ("" "grffile" t)
        ("" "longtable" nil)
        ("" "wrapfig" nil)
        ("" "rotating" nil)
        ("normalem" "ulem" t)
        ("" "amsmath" t)
        ("" "textcomp" t)
        ("" "amssymb" t)
        ("" "capt-of" nil)
        ("" "hyperref" nil)))

(setq org-latex-classes
'(("article"
"\\RequirePackage{fix-cm}
\\PassOptionsToPackage{svgnames}{xcolor}
\\documentclass[11pt]{article}
\\usepackage{fontspec}
\\setmainfont{IBM Plex Sans}
\\setsansfont[Scale=MatchLowercase]{IBM Plex Sans Light}
\\setmonofont[Scale=MatchLowercase]{IBM Plex Mono}
\\usepackage{sectsty}
\\allsectionsfont{\\sffamily}
\\usepackage{enumitem}
\\setlist[description]{style=unboxed,font=\\sffamily\\bfseries}
\\usepackage{listings}
\\lstset{frame=single,aboveskip=1em,
	framesep=.5em,backgroundcolor=\\color{AliceBlue},
	rulecolor=\\color{LightSteelBlue},framerule=1pt}
\\usepackage{xcolor}
\\newcommand\\basicdefault[1]{\\scriptsize\\color{Black}\\ttfamily#1}
\\lstset{basicstyle=\\basicdefault{\\spaceskip1em}}
\\lstset{literate=
	    {§}{{\\S}}1
	    {©}{{\\raisebox{.125ex}{\\copyright}\\enspace}}1
	    {«}{{\\guillemotleft}}1
	    {»}{{\\guillemotright}}1
	    {Á}{{\\'A}}1
	    {Ä}{{\\\"A}}1
	    {É}{{\\'E}}1
	    {Í}{{\\'I}}1
	    {Ó}{{\\'O}}1
	    {Ö}{{\\\"O}}1
	    {Ú}{{\\'U}}1
	    {Ü}{{\\\"U}}1
	    {ß}{{\\ss}}2
	    {à}{{\\`a}}1
	    {á}{{\\'a}}1
	    {ä}{{\\\"a}}1
	    {é}{{\\'e}}1
	    {í}{{\\'i}}1
	    {ó}{{\\'o}}1
	    {ö}{{\\\"o}}1
	    {ú}{{\\'u}}1
	    {ü}{{\\\"u}}1
	    {¹}{{\\textsuperscript1}}1
            {²}{{\\textsuperscript2}}1
            {³}{{\\textsuperscript3}}1
	    {ı}{{\\i}}1
	    {—}{{---}}1
	    {’}{{'}}1
	    {…}{{\\dots}}1
            {⮠}{{$\\hookleftarrow$}}1
	    {␣}{{\\textvisiblespace}}1,
	    keywordstyle=\\color{DarkGreen}\\bfseries,
	    identifierstyle=\\color{DarkRed},
	    commentstyle=\\color{Gray}\\upshape,
	    stringstyle=\\color{DarkBlue}\\upshape,
	    emphstyle=\\color{Chocolate}\\upshape,
	    showstringspaces=false,
	    columns=fullflexible,
	    keepspaces=true}
\\usepackage[a4paper,margin=1in,left=0.9in]{geometry}
\\pagenumbering{arabic}
\\usepackage{parskip}
\\makeatletter
\\renewcommand{\\maketitle}{%
  \\begingroup\\parindent0pt
  \\sffamily
  \\Huge{\\bfseries\\@title}\\par\\bigskip
  \\LARGE{\\bfseries\\@author}\\par\\medskip
  \\normalsize\\@date\\par\\bigskip
  \\endgroup\\@afterindentfalse\\@afterheading}
\\makeatother
[DEFAULT-PACKAGES]
\\hypersetup{linkcolor=Blue,urlcolor=DarkBlue,
  citecolor=DarkRed,colorlinks=true}
\\AtBeginDocument{\\renewcommand{\\UrlFont}{\\ttfamily}}
[PACKAGES]
[EXTRA]"
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}")
("\\paragraph{%s}" . "\\paragraph*{%s}")
("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

("report" "\\documentclass[11pt]{report}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

("book" "\\documentclass[11pt]{book}"
("\\part{%s}" . "\\part*{%s}")
("\\chapter{%s}" . "\\chapter*{%s}")
("\\section{%s}" . "\\section*{%s}")
("\\subsection{%s}" . "\\subsection*{%s}")
("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
