;;; config.el --- org-reftex layer packages file
;;

(defconst org-reftex-packages '(org reftex))

(defun org-autoclock/post-init-org ()
  (use-package org
    :config

;;;
;;; RefTeX cite format
;;;
    (with-eval-after-load 'org
      (defun org-mode-reftex-setup ()
        ;; (load-library "reftex")
        (require 'reftex)
        (and (buffer-file-name)
             (file-exists-p (buffer-file-name))
             (reftex-parse-all)
             )
        (make-local-variable 'reftex-cite-format)
        (setq reftex-cite-format "[[cite:%l]]")
        (setq reftex-default-bibliography '("references"))
        (define-key org-mode-map (kbd "C-c )") 'reftex-citation))
      (add-hook 'org-mode-hook 'org-mode-reftex-setup))

;;;
;;; Key binding
;;;
    (spacemacs/set-leader-keys-for-major-mode 'org-mode "ir" 'org-reftex-citation)

;;;
;;; Link type for citations in latex
;;;
    (org-add-link-type
     "cite" nil
     (lambda (path desc format)
       "Export [[cite:cohen93]] as \cite{cohen93} in LaTeX."
       (if (eq format 'latex)
           (if (or (not desc) (equal 0 (search "cite:" desc)))
               (format "\\cite{%s}" path)
             (format "\\cite[%s]{%s}" desc path)))))

    (org-add-link-type
     "citet" nil
     (lambda (path desc format)
       "Export [[citet:cohen93]] as \citet{cohen93} in LaTeX."
       (if (eq format 'latex)
           (if (or (not desc) (equal 0 (search "citet:" desc)))
               (format "\\citet{%s}" path)
             (format "\\citet[%s]{%s}" desc path)))))

    (org-add-link-type
     "citeauthor" nil
     (lambda (path desc format)
       "Export [[citeauthor:cohen93]] as \citeauthor{cohen93} in LaTeX."
       (if (eq format 'latex)
           (if (or (not desc) (equal 0 (search "citeauthor:" desc)))
               (format "\\citeauthor{%s}" path)
             (format "\\citeauthor[%s]{%s}" desc path)))))

    (org-add-link-type
     "autoref" nil
     (lambda (path desc format)
       "Export [[autoref:lst:Reduction]] as \autoref{lst:Reduction} in LaTeX."
       (if (eq format 'latex)
           (if (or (not desc) (equal 0 (search "autoref:" desc)))
               (format "\\autoref{%s}" path)
             (format "\\autoref[%s]{%s}" desc path)))))

;;;
;;; Run BibTeX
;;;
;;; http://tex.stackexchange.com/a/39885/16685
;;; http://tex.stackexchange.com/a/161619/16685
    (setq org-latex-pdf-process (list "latexmk -pdf %f"))

))
