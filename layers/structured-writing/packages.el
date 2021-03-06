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

(spacemacs/set-leader-keys-for-major-mode 'org-mode "in" 'org-add-note)

;;; SPC m r
;;;
;;; Keys in use:
;;;
;;; 1   2  |3|  4   5   6   7   8   9   0
;;;   q   w  |e|  r  |t|  z   u  |i| |o| |p|
;;;   |a| |s| |d|  f   g   h   j   k  |l|
;;;     y   x   c   v   b  |n|  n   m
;;;

(make-my-org-prepare-export-functions "a"
                                      "structure"
                                      "3"
                                      '(my-org-prepare-unique-tag "Structure")
                                      my-org-publish-properties-structure)

(make-my-org-prepare-export-functions "b"
                                      "purpose"
                                      "p"
                                      (my-org-prepare-unique-tag "Purpose")
                                      my-org-publish-properties-purpose)

(make-my-org-prepare-export-functions "c"
                                      "objectives"
                                      "o"
                                      (my-org-prepare-unique-tag "Objectives")
                                      my-org-publish-properties-objectives)

(make-my-org-prepare-export-functions "d"
                                      "survey"
                                      "d"
                                      (my-org-prepare-multi-tag "Survey")
                                      my-org-publish-properties-survey)

(make-my-org-prepare-export-functions "e"
                                      "ideation"
                                      "i"
                                      (my-org-prepare-multi-tag "Ideation")
                                      my-org-publish-properties-ideation)

(make-my-org-prepare-export-functions "f"
                                      "strategy"
                                      "s"
                                      (my-org-prepare-unique-tag "Strategy")
                                      my-org-publish-properties-strategy)

(make-my-org-prepare-export-functions "g"
                                      "literature"
                                      "l"
                                      (my-org-prepare-unique-parental-tag "Literature" "Reference")
                                      my-org-publish-properties-literature)

(make-my-org-prepare-export-functions "h"
                                      "examples"
                                      "e"
                                      (my-org-prepare-unique-tag "Examples")
                                      my-org-publish-properties-examples)

(make-my-org-prepare-export-functions "i"
                                      "scratch"
                                      "a"
                                      (my-org-prepare-unique-tag "Scratch")
                                      my-org-publish-properties-scratch)

(make-my-org-prepare-export-functions "j"
                                      "notes"
                                      "n"
                                      (my-org-prepare-unique-tag "Notes")
                                      my-org-publish-properties-notes)

(make-my-org-prepare-export-functions "k"
                                      "text"
                                      "t"
                                      (my-org-prepare-unique-tag "Text")
                                      my-org-publish-properties-text
                                      t)

(spacemacs/set-leader-keys-for-major-mode 'org-mode "ru" 'my-org-setup-outline-publish)
(spacemacs/set-leader-keys-for-major-mode 'org-mode "uu" 'my-org-publish-outline)

))
