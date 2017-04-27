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
                                (org-forward-heading-same-level 1)
                                (when (eq beg (point))
                                  (if (> (org-outline-level) 1)
                                    (progn
                                      (outline-up-heading 1)
                                      (org-forward-heading-same-level 1))
                                    (goto-char (point-max)))
                                  )
                                (backward-char)
                                (delete-region beg (point))))
                   (mapconcat 'identity tags "|")
                   'nil)
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

(defun my-org-prepare-current-outline-subtree ()
  ""
  ; Remove heading
  ;(delete-region (point-at-bol) (point-at-eol))
  (org-set-tags "no_title")
  ; Promote children to top level
  (org-map-entries (lambda () (org-promote))
                   (mapconcat 'identity (remove "Outline" my-org-tags) "|")
                   'tree)
  )

(defun my-org-prepare-outline-subtree (heading-id)
  (show-all)
  ; Add :Outline: section with ID heading-id to export set
  (org-map-entries (lambda ()
                     (let ((‘org-trust-scanner-tags’ t))
                       (let ((properties (org-entry-properties)))
                         (when (equal heading-id (cdr (assoc "ID" properties)))
                           (my-org-prepare-current-outline-subtree)))))
                   "Outline"
                   'nil)
  ; Remove all sections tagged :Outline:
  (let ((tags-to-remove my-org-tags))
    (delete "Purpose" tags-to-remove)
    (delete "Objectives" tags-to-remove)
    (delete "Strategy" tags-to-remove)
    (delete "Notes" tags-to-remove)
    (delete "Text" tags-to-remove)
    (delete "References" tags-to-remove)
    (delete "Appendix" tags-to-remove)
    (my-org-remove-section tags-to-remove)
    )
  ; Remove headings from references and appendix sections
  (my-org-remove-heading '("References" "Appendix"))
  )

;; when maketitle is t the document is required to place the `'\maketitle`
(defmacro make-my-org-prepare-export-functions (no name keybinding preparation properties &optional maketitle)
  `(progn
     (defun ,(intern (format "my-org-prepare-export-%s" name)) (backend)
       ,preparation
       )
     (defun ,(intern (format "my-org-enable-prepare-export-%s" name)) (argument)
       (add-hook 'org-export-before-parsing-hook
                 #',(intern (format "my-org-prepare-export-%s" name)))
       (when ,maketitle (setq org-latex-title-command ""))
       )
     (defun ,(intern (format "my-org-disable-prepare-export-%s" name)) (argument)
       (remove-hook 'org-export-before-parsing-hook
                    #',(intern (format "my-org-prepare-export-%s" name)))
       (when ,maketitle (setq org-latex-title-command "\\maketitle"))
       )
     (defun ,(intern (format "my-org-latex-publish-to-pdf-%s" name)) (plist filename pub-dir)
       (org-publish-attachment
        plist
        (org-latex-compile
         (org-publish-org-to 'latex
                             filename ,(format ".%s.%s.tex" no name)
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
        ,(format "r%s" keybinding) ',(intern (format "my-org-publish-%s" name)))

))

(require 'calc-bin)

(defun my-org-convert-number (number)
  "Convert number to radix 36 and return as string"
  (let ((calc-number-radix 36))
                      (math-format-radix number)))

(defmacro make-my-org-prepare-section-export-functions (xoutline-path xheading-id xheading-name xheading-path xhas-children)
  "Prepare export functions for :Outline: section export.
xoutline-path is expected to contain the section numbering as a
list. xheading-id is expected to be a string containing the
unique ID property of that heading. xheading-name the name of
that heading. xheading-path is expected to contain the headings
on the path to that heading including itself. xhas-children
indicates whether there are sub-sections tagged with :Outline:."
  `(progn
     ;,(message "%s\t%s\t%s\t%s"
     ;          (eval xoutline-path)
     ;          (eval xheading-id)
     ;          (eval xheading-name)
     ;          (eval xhas-children)
     ;          )
     (defun ,(intern (format "my-org-prepare-export-%s" (eval xheading-id))) (backend)
       (my-org-prepare-outline-subtree ,(eval xheading-id))
       )
     (defun ,(intern (format "my-org-enable-prepare-export-%s" (eval xheading-id))) (argument)
       (add-hook 'org-export-before-parsing-hook
                 #',(intern (format "my-org-prepare-export-%s" (eval xheading-id))))
       )
     (defun ,(intern (format "my-org-disable-prepare-export-%s" (eval xheading-id))) (argument)
       (remove-hook 'org-export-before-parsing-hook
                    #',(intern (format "my-org-prepare-export-%s" (eval xheading-id))))
       )
     (defun ,(intern (format "my-org-latex-publish-to-pdf-%s" (eval xheading-id))) (plist filename pub-dir)
       (setq org-export-with-broken-links t)
       (org-publish-attachment
        plist
        (org-latex-compile
         (org-publish-org-to 'latex
                             filename (format ".%s.%s.tex" ,(mapconcat 'my-org-convert-number (eval xoutline-path) ".") ,(eval xheading-name))
                             plist (file-name-directory filename)))
        pub-dir)
       (setq org-export-with-broken-links nil))
     (defun ,(intern (format "my-org-publish-%s" (eval xheading-id))) ()
       (interactive)
       (save-excursion
          (let ((general-properties '(,(concat (file-name-base (buffer-file-name (buffer-base-buffer))) "-" (eval xheading-id))
                                      :base-directory ,(file-name-directory (buffer-file-name (buffer-base-buffer)))
                                      :base-extension "org"
                                      :publishing-directory ,(file-name-directory (buffer-file-name (buffer-base-buffer)))
                                      :preparation-function ,(intern (format "my-org-enable-prepare-export-%s" (eval xheading-id)))
                                      :completion-function ,(intern (format "my-org-disable-prepare-export-%s" (eval xheading-id)))
                                      :publishing-function ,(intern (format "my-org-latex-publish-to-pdf-%s" (eval xheading-id)))
                                      :include (,(file-name-nondirectory (buffer-file-name (buffer-base-buffer))))
                                      :exclude "\\.org$"
                                      :subtitle ,(mapconcat 'identity (eval xheading-path) " -- ")
                                      )))
            (org-publish (append general-properties my-org-publish-properties-base))
            )
          )
       )
     (defun ,(intern (format "my-org-publish-%s-%s"
                             (mapconcat 'my-org-convert-number (eval xoutline-path) "")
                             (eval xheading-name))) ()
       (interactive)
       ,(intern (format "my-org-publish-%s" (eval xheading-id)))
       )
     (let ((kbd ,(mapconcat 'my-org-convert-number (eval xoutline-path) "")))
       (if ,(eval xhas-children)
         ; TODO: Try to find out how `spacemacs/declare-prefix-for-mode' works and add a prefix
         (spacemacs/set-leader-keys-for-major-mode
           'org-mode (concat "u" kbd "0")
           ',(intern (format "my-org-publish-%s-%s"
                             (mapconcat 'my-org-convert-number (eval xoutline-path) "")
                             (eval xheading-name))))
         (spacemacs/set-leader-keys-for-major-mode
           'org-mode (concat "u" kbd)
           ',(intern (format "my-org-publish-%s-%s"
                             (mapconcat 'my-org-convert-number (eval xoutline-path) "")
                             (eval xheading-name))))
         )
       )
     )
  )

(defun my-org-setup-outline-publish (&optional outline-path heading-path)
  ""
  (interactive)
  (unless outline-path
    (setq outline-path '()))
  (unless heading-path
    (setq heading-path '()))
  (save-excursion
    (let ((inc-variable 0)
          (sections-seen 0))
      (org-map-entries
       '(lambda ()
          (when (equal heading-path (org-get-outline-path))
            (setq inc-variable (+ inc-variable 1))
            (let ((outline-path (append outline-path (list inc-variable))))
              (save-restriction
                (org-narrow-to-subtree)
                (let ((children (my-org-setup-outline-publish outline-path (org-get-outline-path t))))
                  (let ((‘org-trust-scanner-tags’ t))
                    (let ((properties (org-entry-properties)))
                      (let ((heading-id (cdr (assoc "ID" properties)))
                            (heading-name (cdr (assoc "ITEM" properties)))
                            (has-children (> children 0))
                            (heading-path (org-get-outline-path t)))
                        ; (message "Path: %s\n\tLevel: %s\n\tHeading: %s\n\tID: %s\n\tOutline path: %s\n\tChildren: %s (%s)"
                        ;          outline-path
                        ;          (org-current-level)
                        ;          heading-name
                        ;          heading-id
                        ;          (org-get-outline-path t)
                        ;          children
                        ;          has-children
                        ;          )
                        (make-my-org-prepare-section-export-functions outline-path
                                                                      heading-id
                                                                      heading-name
                                                                      heading-path
                                                                      has-children
                                                                      )
                        (setq sections-seen (+ sections-seen 1 children))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          "Outline"
          'nil
          )
      (message "Setup publishing functions for %s outline sections" sections-seen)
      inc-variable
      )
    )
  )

(defun my-org-publish-outline ()
  (interactive)
  (my-org-setup-outline-publish)
  (let ((sections-seen 0))
    (org-map-entries (lambda ()
                       (let ((‘org-trust-scanner-tags’ t))
                         (let ((properties (org-entry-properties)))
                           (let ((heading-id (cdr (assoc "ID" properties))))
                             (when heading-id
                               (funcall (intern (format "my-org-publish-%s" heading-id)))
                               (setq sections-seen (+ sections-seen 1)))))))
                     "Outline"
                     'nil)
    (message "Published documents for %s outline sections" sections-seen)
    )
  )
