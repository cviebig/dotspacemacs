;; C-S-left, C-S-right, select text and move cursor
(setq org-support-shift-select 't)

;;; http://stackoverflow.com/questions/698562
(setq org-export-with-sub-superscripts nil)

(setq org-drawers (quote ("PROPERTIES" "LOGBOOK" "NOTES" "CLOCKTABLE" "EXPOSEABSTRACT" "DOCUMENTSCOPE")))

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

; Got hint about this at
; https://lists.gnu.org/archive/html/emacs-orgmode/2016-01/msg00042.html
(remove-hook 'org-clock-out-hook 'org-clock-remove-empty-clock-drawer)

(setq org-export-with-drawers 'nil)
(setq org-export-with-tags 'nil)

;; log into drawer too
;; #+TODO: TODO(!) CURRENT(!/!) WAITING(@) HOLD(@) CANCELLED(c@) DONE(!)
(setq org-log-into-drawer t)

(defun org-clocktable-indent-string (level)
  (if (= level 1) ""
    (let ((str " "))
      (dotimes (k (1- level) str)
	;;      (setq str (concat "\\emsp" str))))))
	(setq str (concat ".." str))))))

;;; http://stackoverflow.com/questions/10635989/emacs-org-agenda-list-destroy-my-windows-splits
;;; leave agenda view with x
(setq org-agenda-window-setup 'current-window) 

;;; enable python execution
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t) (sh . t)))

;;; enable syntax highlighting
(setq org-src-fontify-natively t)

;;; evaluate python code without explicit confirmation
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "python")))  ; don't ask for python
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;;; Do not evaluate code blocks on export
;;; see http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
(setq org-export-babel-evaluate nil)

;; format code blocks using their native major mode
;; http://www.i3s.unice.fr/~malapert/org/tips/emacs_orgmode.html
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;;; http://www.emacswiki.org/emacs/SmoothScrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)

(setq org-ellipsis "…")

(spacemacs/set-leader-keys-for-major-mode 'org-mode "tol" 'org-toggle-link-display)
