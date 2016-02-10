;;; config.el --- structured writing layer configuration file
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Prerequisites

(configuration-layer/declare-layers '(org))

;;; Code:

(setq my-org-tags '("Structure"
                    "Survey"
                    "Goals"
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

(make-my-org-prepare-export-functions "structure"
                                      "3"
                                      '(my-org-prepare-unique-tag "Structure")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "survey"
                                      "w"
                                      (my-org-prepare-multi-tag "Survey")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "goals"
                                      "g"
                                      (my-org-prepare-unique-tag "Goals")
                                      my-org-publish-properties-text)

(make-my-org-prepare-export-functions "ideation"
                                      "s"
                                      (my-org-prepare-multi-tag "Ideation")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "strategy"
                                      "t"
                                      (my-org-prepare-unique-tag "Strategy")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "literature"
                                      "e"
                                      (my-org-prepare-unique-parental-tag "Literature" "Reference")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "examples"
                                      "q"
                                      (my-org-prepare-unique-tag "Examples")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "scratch"
                                      "a"
                                      (my-org-prepare-unique-tag "Scratch")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "notes"
                                      "d"
                                      (my-org-prepare-unique-tag "Notes")
                                      my-org-publish-properties-others)

(make-my-org-prepare-export-functions "text"
                                      "f"
                                      (my-org-prepare-unique-tag "Text")
                                      my-org-publish-properties-text)
