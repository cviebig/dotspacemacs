;;;
;;;
;;;
(setq org-tags-exclude-from-inheritance
'("Structure" "Emacs" "Code" "Build" "Tutorial" "Pipeline" "Expose" "Intro" "Goals" "Strategy" "Literature" "Notes" "Text" "References"))

(defun org-prepare-export-structure (backend)
  ""
  (show-all)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "References")
)

;;; code to promote only the children was found in
;;; http://orgmode.org/worg/org-hacks.html Promote all items in subtree
(defun org-prepare-export-emacs (backend)
  ""
  (show-all)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Emacs|References")
  (org-map-entries (lambda () (org-promote-subtree))
                   "E")
)

(defun org-prepare-export-code (backend)
  ""
  (show-all)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Code|References")
  (org-map-entries (lambda () (org-promote-subtree))
                   "C")
)

(defun org-prepare-export-build (backend)
  ""
  (show-all)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Build|References")
  (org-map-entries (lambda () (org-promote-subtree))
                   "B")
)

(defun org-prepare-export-tutorial (backend)
  ""
  (show-all)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Tutorial|References")
  (org-map-entries (lambda () (org-promote-subtree))
                   "T")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Scratch" tree)
)

(defun org-prepare-export-pipeline (backend)
  ""
  (show-all)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Pipeline|References")
  (org-map-entries (lambda () (org-promote-subtree))
                   "P")
)

(defun org-prepare-export-survey (backend)
  "Remove sections with :Goals|Strategy|Literature|Notes: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Goals|Scratch|Strategy|Literature|Reference|Notes|Note|Text" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "References|Appendix")
)

(defun org-prepare-export-goals (backend)
  "Remove sections with :Goals|Strategy|Notes|Text: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Survey|Strategy|Scratch|Literature|Reference|Notes|Note|Text" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Goals|References|Appendix")
)

(defun org-prepare-export-scratchpad (backend)
  "Remove sections with :Goals|Strategy|Notes|Text: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Survey|Goals|Strategy|Literature|Reference|Notes|Note|Text" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "References|Appendix")
)

(defun org-prepare-export-strategy (backend)
  "Remove sections with :Literature|Notes|Text: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Survey|Goals|Scratch|Literature|Reference|Notes|Note|Text" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Strategy|References|Appendix")
)

(defun org-prepare-export-literature (backend)
  "Remove sections with :Goals|Strategy|Notes|Text: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Survey|Goals|Scratch|Strategy|Notes|Note|Text" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Literature|References|Appendix")
  (org-map-entries (lambda () (org-promote))
                   "Reference")
)

(defun org-prepare-export-notes (backend)
  "Remove sections with :Goals|Strategy|Literature|Text: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Survey|Goals|Scratch|Strategy|Literature|Reference|Text" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Notes|References|Appendix")
  (org-map-entries (lambda () (org-promote))
                   "Note")
)

(defun org-prepare-export-text (backend)
  "Remove sections with :Goals|Strategy|Literature|Notes: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "Content")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Expose")
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   "Survey|Goals|Scratch|Strategy|Literature|Reference|Notes|Note" tree)
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Text|References|Appendix")
)

(defun org-prepare-export-intro (backend)
  "Remove sections with :Goals|Strategy|Notes|Text: tag."
  (show-all)
  (org-map-entries (lambda () (org-promote))
                   "I")
  (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                   "Intro|References")
)

(defun org-enable-prepare-export-structure () (add-hook 'org-export-before-processing-hook #'org-prepare-export-structure))
(defun org-disable-prepare-export-structure () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-structure))

(defun org-enable-prepare-export-emacs () (add-hook 'org-export-before-processing-hook #'org-prepare-export-emacs))
(defun org-disable-prepare-export-emacs () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-emacs))

(defun org-enable-prepare-export-code () (add-hook 'org-export-before-processing-hook #'org-prepare-export-code))
(defun org-disable-prepare-export-code () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-code))

(defun org-enable-prepare-export-build () (add-hook 'org-export-before-processing-hook #'org-prepare-export-build))
(defun org-disable-prepare-export-build () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-build))

(defun org-enable-prepare-export-tutorial () (add-hook 'org-export-before-processing-hook #'org-prepare-export-tutorial))
(defun org-disable-prepare-export-tutorial () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-tutorial))

(defun org-enable-prepare-export-pipeline () (add-hook 'org-export-before-processing-hook #'org-prepare-export-pipeline))
(defun org-disable-prepare-export-pipeline () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-pipeline))

(defun org-enable-prepare-export-survey () (add-hook 'org-export-before-processing-hook #'org-prepare-export-survey))
(defun org-disable-prepare-export-survey () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-survey))

(defun org-enable-prepare-export-goals () (add-hook 'org-export-before-processing-hook #'org-prepare-export-goals))
(defun org-disable-prepare-export-goals () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-goals))

(defun org-enable-prepare-export-scratchpad () (add-hook 'org-export-before-processing-hook #'org-prepare-export-scratchpad))
(defun org-disable-prepare-export-scratchpad () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-scratchpad))

(defun org-enable-prepare-export-strategy () (add-hook 'org-export-before-processing-hook #'org-prepare-export-strategy))
(defun org-disable-prepare-export-strategy () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-strategy))

(defun org-enable-prepare-export-literature () (add-hook 'org-export-before-processing-hook #'org-prepare-export-literature))
(defun org-disable-prepare-export-literature () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-literature))

(defun org-enable-prepare-export-notes () (add-hook 'org-export-before-processing-hook #'org-prepare-export-notes))
(defun org-disable-prepare-export-notes () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-notes))

(defun org-enable-prepare-export-text () (add-hook 'org-export-before-processing-hook #'org-prepare-export-text))
(defun org-disable-prepare-export-text () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-text))

(defun org-enable-prepare-export-intro () (add-hook 'org-export-before-processing-hook #'org-prepare-export-intro))
(defun org-disable-prepare-export-intro () (remove-hook 'org-export-before-processing-hook #'org-prepare-export-intro))

(defun org-latex-publish-to-pdf-structure (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".structure.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-emacs (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".emacs.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-code (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".code.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-build (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".build.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-tutorial (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".tutorial.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-pipeline (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".pipeline.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-survey (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".survey.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-goals (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".goals.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-scratchpad (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".scratch.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-strategy (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".strategy.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-literature (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".literature.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-notes (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".notes.tex" plist (file-name-directory filename)))
   pub-dir))

(defun org-latex-publish-to-pdf-text (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'latex filename ".text.tex" plist (file-name-directory filename)))
   pub-dir))

(require 'ox-beamer)
(defun org-beamer-publish-to-pdf-intro (plist filename pub-dir)
  (org-publish-attachment
   plist
   (org-latex-compile
    (org-publish-org-to
     'beamer filename ".intro.tex" plist (file-name-directory filename)))
   pub-dir))

(setq org-publish-project-alist
      '(
        ("outline-structure"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-structure
	 :completion-function  org-disable-prepare-export-structure
         :publishing-function org-latex-publish-to-pdf-structure
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Structure" "References")
         )
        ("outline-emacs"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-emacs
	 :completion-function  org-disable-prepare-export-emacs
         :publishing-function org-latex-publish-to-pdf-emacs
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Emacs" "E" "References")
         )
        ("outline-code"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-code
	 :completion-function  org-disable-prepare-export-code
         :publishing-function org-latex-publish-to-pdf-code
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Code" "C" "References")
         )
        ("outline-build"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-build
	 :completion-function  org-disable-prepare-export-build
         :publishing-function org-latex-publish-to-pdf-build
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Build" "B" "References")
         )
        ("outline-tutorial"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-tutorial
	 :completion-function  org-disable-prepare-export-tutorial
         :publishing-function org-latex-publish-to-pdf-tutorial
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Tutorial" "T" "References")
         )
        ("outline-pipeline"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-pipeline
	 :completion-function  org-disable-prepare-export-pipeline
         :publishing-function org-latex-publish-to-pdf-pipeline
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Pipeline" "P" "References")
         )
        ("outline-survey"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-survey
	 :completion-function  org-disable-prepare-export-survey
         :publishing-function org-latex-publish-to-pdf-survey
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Content" "References")
         )
        ("outline-goals"
         :base-directory "~/org-mode/"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-goals
	 :completion-function  org-disable-prepare-export-goals
         :publishing-function org-latex-publish-to-pdf-goals
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :latex-class-options "[twocolumn]"
	 :headline-levels 5
	 :with-drawers ("EXPOSEABSTRACT")
	 :with-toc nil
	 :select-tags ("Content" "References")
         )
        ("outline-scratchpad"
         :base-directory "~/org-mode/"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-scratchpad
	 :completion-function  org-disable-prepare-export-scratchpad
         :publishing-function org-latex-publish-to-pdf-scratchpad
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Content" "References")
         )
        ("outline-strategy"
         :base-directory "~/org-mode/"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-strategy
	 :completion-function  org-disable-prepare-export-strategy
         :publishing-function org-latex-publish-to-pdf-strategy
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :latex-class-options "[twocolumn]"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Content" "References")
         )
        ("outline-literature"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-literature
	 :completion-function  org-disable-prepare-export-literature
         :publishing-function org-latex-publish-to-pdf-literature
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Content" "References")
         )
        ("outline-notes"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-notes
	 :completion-function  org-disable-prepare-export-notes
         :publishing-function org-latex-publish-to-pdf-notes
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Content" "References" "Appendix")
         )
        ("outline-text"
         :base-directory "~/org-mode"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-text
	 :completion-function  org-disable-prepare-export-text
         :publishing-function org-latex-publish-to-pdf-text
         :include ("2015-Outline.org")
         :exclude "\\.org$"
         :latex-class "scrartcl"
	 :latex-class-options "[twocolumn]"
	 :headline-levels 5
	 :with-toc nil
	 :select-tags ("Content" "References" "Appendix")
         )
        ("outline-intro"
         :base-directory "~/org-mode/"
         :base-extension "org"
         :publishing-directory "~/org-mode"
	 :preparation-function org-enable-prepare-export-intro
	 :completion-function  org-disable-prepare-export-intro
         :publishing-function org-beamer-publish-to-pdf-intro
         :include ("2015-Outline.org")
         :exclude "\\.org$"
	 :latex-class "beamer"
         :latex-class-options "[presentation,smaller]"
	 :headline-levels 2
	 :beamer-theme "default"
	 :select-tags ("Intro" "I" "References")
         )
        ))

;;; choose t instead of nil as a last argument to make publishing asynchronous
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-crf" '(lambda () (interactive) (org-publish "outline-structure" nil nil)))
            (local-set-key "\C-crm" '(lambda () (interactive) (org-publish "outline-emacs" nil nil)))
            (local-set-key "\C-crc" '(lambda () (interactive) (org-publish "outline-code" nil nil)))
            (local-set-key "\C-crb" '(lambda () (interactive) (org-publish "outline-build" nil nil)))
            (local-set-key "\C-crv" '(lambda () (interactive) (org-publish "outline-tutorial" nil nil)))
            (local-set-key "\C-crd" '(lambda () (interactive) (org-publish "outline-pipeline" nil nil)))
            (local-set-key "\C-crs" '(lambda () (interactive) (org-publish "outline-survey" nil nil)))
            (local-set-key "\C-crg" '(lambda () (interactive) (org-publish "outline-goals" nil nil)))
            (local-set-key "\C-crr" '(lambda () (interactive) (org-publish "outline-scratchpad" nil nil)))
            (local-set-key "\C-cre" '(lambda () (interactive) (org-publish "outline-strategy" nil nil)))
            (local-set-key "\C-crl" '(lambda () (interactive) (org-publish "outline-literature" nil nil)))
            (local-set-key "\C-crn" '(lambda () (interactive) (org-publish "outline-notes" nil nil)))
            (local-set-key "\C-crt" '(lambda () (interactive) (org-publish "outline-text" nil nil)))
            (local-set-key "\C-cri" '(lambda () (interactive) (org-publish "outline-intro" nil nil)))
          ))
