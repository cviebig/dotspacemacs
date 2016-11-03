;;; config.el --- structured writing layer packages file
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst structured-writing-packages '(org))

(defun structured-writing/post-init-org ()
  (use-package org
    :config

;;; SPC m r
;;;
;;; Keys in use:
;;;
;;; 1   2  |3|  4   5   6   7   8   9   0
;;;  |q| |w| |e|  r  |t|  z   u   i   o   p
;;;   |a| |s| |d| |f| |g|  h   j   k   l
;;;     y   x   c   v   b   n   n   m
;;;

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

))
