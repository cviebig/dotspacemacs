;;; config.el --- org-autoclock layer packages file
;;

(defconst org-autoclock-packages '(org))

(defun org-autoclock/post-init-org ()
  (use-package org
    :config

  (setq my-org-autoclock-current-section nil)
  (setq my-org-autoclock-initialized nil)
  (setq my-org-autoclock-enabled nil)
  (make-variable-buffer-local 'my-org-autoclock-initialized)
  (make-variable-buffer-local 'my-org-autoclock-enabled)

  (defun my-org-autoclock-enable ()
    (interactive)
    (setq my-org-autoclock-initialized t)
    (setq my-org-autoclock-enabled t)
    (print "Enable autoclocking"))

  (defun my-org-autoclock-disable ()
    (interactive)
    (setq my-org-autoclock-initialized t)
    (setq my-org-autoclock-enabled nil)
    (print "Disable autoclocking"))

  (defun my-org-autoclock-toggle ()
    (interactive)
    (setq my-org-autoclock-initialized t)
    (if my-org-autoclock-enabled
        (progn (setq my-org-autoclock-enabled nil)
               (print "Disable autoclocking"))
        (progn (setq my-org-autoclock-enabled t)
               (print "Enable autoclocking"))))

  (defun my-org-autoclock-section ()
    ""
    (when (eq major-mode 'org-mode)
      (unless my-org-autoclock-initialized
        (when (org-entry-get nil "autoclock" t)
          (setq my-org-autoclock-enabled t))
        (setq my-org-autoclock-initialized t))
      (when my-org-autoclock-enabled
        (if (org-before-first-heading-p)
            (progn (when (org-clocking-p) (org-clock-out)))
            (let ((my-org-current-section (org-id-get nil t)))
              (unless (and (equal my-org-autoclock-current-section my-org-current-section)
                           (org-clocking-p))
                (org-clock-in)
                (setq my-org-autoclock-current-section my-org-current-section)))))))

  (add-hook 'post-command-hook 'my-org-autoclock-section)

  (spacemacs/set-leader-keys-for-major-mode 'org-mode "k" 'my-org-autoclock-toggle)

  ; Instead of (org-id-get nil t) it might be reasonable to think about
  ; choosing (org-get-outline-path t t) for identifying a section. This
  ; would avoid having to generate a unique ID for every section in our
  ; documents.

))
