;;; config.el --- pensieve layer configuration file
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Prerequisites

(configuration-layer/declare-layers '(org))

;;; Code:

(setq org-agenda-files '("~/projects/pensieve"))
