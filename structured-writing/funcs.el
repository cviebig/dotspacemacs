;;; funcs.el --- structured writing layer functions file
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defun my-org-remove-heading (tags)
  ""
 (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                  (mapconcat 'identity tags "|"))
)

(defun my-org-remove-section (tags)
  ""
  (org-map-entries (lambda () (let ((beg (point)))
                                (outline-next-visible-heading 1)
                                (backward-char)
                                (delete-region beg (point))))
                   (mapconcat 'identity tags "|")
                   tree)
)

(defun my-org-prepare-multi-tag (tag)
  ""
  (show-all)
  (my-org-remove-section (remove tag my-org-tags))
  (my-org-remove-heading '("References" "Appendix"))
)

(defun my-org-prepare-unique-tag (tag)
  ""
  (show-all)
  (my-org-remove-section (remove tag my-org-tags))
  (my-org-remove-heading (cons tag '("References" "Appendix")))
)

(defun my-org-prepare-unique-parental-tag (tag children)
  ""
  (show-all)
  (my-org-remove-section (remove tag my-org-tags))
  (my-org-remove-heading (cons tag '("References" "Appendix")))
  (org-map-entries (lambda () (org-promote)) children)
)

(defmacro make-my-org-prepare-export-functions (name key preparation properties)
  `(progn
     (defun ,(intern (format "my-org-prepare-export-%s" name)) (backend)
       ""
       ,preparation
       )
     (defun ,(intern (format "my-org-enable-prepare-export-%s" name)) ()
       (add-hook 'org-export-before-processing-hook
                 #',(intern (format "my-org-prepare-export-%s" name)))
       )
     (defun ,(intern (format "my-org-disable-prepare-export-%s" name)) ()
       (remove-hook 'org-export-before-processing-hook
                    #',(intern (format "my-org-prepare-export-%s" name)))
       )
     (defun ,(intern (format "my-org-latex-publish-to-pdf-%s" name)) (plist filename pub-dir)
       (org-publish-attachment
        plist
        (org-latex-compile
         (org-publish-org-to 'latex
                             filename ,(format ".%s.tex" name)
                             plist (file-name-directory filename)))
        pub-dir))
     (defun ,(intern (format "my-org-publish-%s" name)) ()
       (interactive)
       (save-excursion
        (let ((name ,name) (custom-properties ,properties))
          (let ((general-properties `(,(concat (file-name-base (buffer-file-name (buffer-base-buffer))) "-" name)
                                      :base-directory ,(file-name-directory (buffer-file-name (buffer-base-buffer)))
                                      :base-extension "org"
                                      :publishing-directory ,(file-name-directory (buffer-file-name (buffer-base-buffer)))
                                      :preparation-function ,(intern (format "my-org-enable-prepare-export-%s" name))
                                      :completion-function ,(intern (format "my-org-disable-prepare-export-%s" name))
                                      :publishing-function ,(intern (format "my-org-latex-publish-to-pdf-%s" name))
                                      :include (,(file-name-nondirectory (buffer-file-name (buffer-base-buffer))))
                                      :exclude "\\.org$"
                                      )))
            (org-publish (append general-properties custom-properties))
        ))))
     (spacemacs/set-leader-keys-for-major-mode 'org-mode
        ,(format "r%s" key) ',(intern (format "my-org-publish-%s" name)))

))
