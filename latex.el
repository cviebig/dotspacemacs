;;;
;;; Add scrartcl class to latex export
;;;
(unless (find "scrartcl" org-latex-classes :key 'car
          :test 'equal)
  (add-to-list 'org-latex-classes
           '("scrartcl"
              "\\documentclass{scrartcl}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage{url}
\\usepackage[numbers]{natbib}
\\usepackage[hidelinks,backref]{hyperref}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{listings}
\\usepackage[usenames,dvipsnames]{xcolor}
\\usepackage{booktabs}
              [NO-DEFAULT-PACKAGES]
              [EXTRA]"
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}
\\usepackage[T1]{fontenc}
\\usepackage[utf8]{inputenc}
\\usepackage{lmodern}
\\usepackage{url}
\\usepackage{natbib}
\\usepackage{hyperref}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{listings}
\\usepackage{xcolor}
\\usepackage{booktabs}
              [NO-DEFAULT-PACKAGES]
              [EXTRA]"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;;; Enable booktabs for nicer table rules
;;; http://lists.gnu.org/archive/html/emacs-orgmode/2012-12/msg00971.html
(setq org-latex-tables-booktabs t)

;;; Setup listings export
;;; http://orgmode.org/worg/org-tutorials/org-latex-export.html
(setq org-latex-listings 'listings)
(setq org-latex-listings-options
      '(("frame" "tb")
        ("basicstyle" "\\small")
	("breaklines" "true")
        ("numbers" "left")
        ("numberstyle" "\\tiny")
	("captionpos" "b")))

