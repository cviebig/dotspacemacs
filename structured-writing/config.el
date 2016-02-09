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
                    "Examples"
                    "Scratch"
                    "Notes"
                    "Text"
                    "References"))

(setq org-tags-exclude-from-inheritance my-org-tags)

(make-my-org-prepare-export-functions "structure"
                                      '(my-org-prepare-unique-tag "Structure")
                                       "3")
(make-my-org-prepare-export-functions "survey"
                                      (my-org-prepare-multi-tag "Survey")
                                      "w")
(make-my-org-prepare-export-functions "goals"
                                      (my-org-prepare-unique-tag "Goals")
                                      "g")
(make-my-org-prepare-export-functions "ideation"
                                      (my-org-prepare-multi-tag "Ideation")
                                      "s")
(make-my-org-prepare-export-functions "strategy"
                                      (my-org-prepare-unique-tag "Strategy")
                                      "t")
(make-my-org-prepare-export-functions "literature"
                                      (my-org-prepare-unique-parental-tag "Literature" "Reference")
                                      "e")
(make-my-org-prepare-export-functions "examples"
                                      (my-org-prepare-unique-tag "Examples")
                                      "q")
(make-my-org-prepare-export-functions "scratch"
                                      (my-org-prepare-unique-tag "Scratch")
                                      "a")
(make-my-org-prepare-export-functions "notes"
                                      (my-org-prepare-unique-tag "Notes")
                                      "d")
(make-my-org-prepare-export-functions "text"
                                      (my-org-prepare-unique-tag "Text")
                                      "f")
