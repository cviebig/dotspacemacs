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
(setq my-org-publish-properties-text '(
    :latex-class "scrartcl"
    :latex-class-options "[twocolumn]"
    :headline-levels 5
    :with-drawers ("EXPOSEABSTRACT")
    :with-toc nil
))

(setq my-org-publish-properties-others '(
    :latex-class "scrartcl"
    :headline-levels 5
    :with-toc nil
))
