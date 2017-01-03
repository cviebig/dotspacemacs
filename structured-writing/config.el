;;; config.el --- structured writing layer configuration file
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq my-org-tags '("Structure"
                    "Purpose"
                    "Survey"
                    "Objectives"
                    "Ideation"
                    "Strategy"
                    "Literature"
                    "Reference"
                    "Examples"
                    "Scratch"
                    "Notes"
                    "Text"))

(setq org-tags-exclude-from-inheritance my-org-tags)

;;; properties that will be merged into the
;;; on-the-fly generated org-publish-project-alist.
(setq my-org-publish-properties-base '(
    :latex-class "scrartcl"
    :headline-levels 5
    :with-toc nil
))

(setq my-org-publish-properties-twocolumn
      (append '(:latex-class-options "[twocolumn]")
              my-org-publish-properties-base))

(setq my-org-publish-properties-structure
      (append '(:subtitle "Document structure")
              my-org-publish-properties-base))
(setq my-org-publish-properties-purpose
      (append '(:subtitle "Purpose")
              my-org-publish-properties-twocolumn))
(setq my-org-publish-properties-survey
      (append '(:subtitle "Surveys")
              my-org-publish-properties-base))
(setq my-org-publish-properties-objectives
      (append '(:subtitle "Objectives")
              my-org-publish-properties-twocolumn))
(setq my-org-publish-properties-ideation
      (append '(:subtitle "Ideation")
              my-org-publish-properties-base))
(setq my-org-publish-properties-strategy
      (append '(:subtitle "Strategy")
              my-org-publish-properties-twocolumn))
(setq my-org-publish-properties-literature
      (append '(:subtitle "Literature")
              my-org-publish-properties-base))
(setq my-org-publish-properties-examples
      (append '(:subtitle "Examples")
              my-org-publish-properties-base))
(setq my-org-publish-properties-scratch
      (append '(:subtitle "Scratch")
              my-org-publish-properties-base))
(setq my-org-publish-properties-notes
      (append '(:subtitle "Notes")
              my-org-publish-properties-base))
(setq my-org-publish-properties-text
      (append '(:with-drawers ("EXPOSEABSTRACT"))
              my-org-publish-properties-twocolumn))
