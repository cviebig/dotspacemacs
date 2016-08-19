;;; c++ mode settings
;;; http://stackoverflow.com/a/663636/1531656
;;; http://stackoverflow.com/a/22711444/1531656
(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'innamespace [4])
  (c-set-offset 'substatement-open 0)
  )
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (setq indent-tabs-mode nil)
  (c-set-offset 'substatement-open 0)
  )
(add-hook 'c-mode-hook 'my-c-mode-hook)
