;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'yes-or-no-p #'y-or-n-p)

(use-package goto-addr
  :ensure nil
  :hook (prog-mode . goto-address-prog-mode)
  :config
  (setq goto-address-mail-face 'link))

(use-package shift-number
  :bind (("C-c +" . shift-number-up)
         ("C-c -" . shift-number-down)))

;; Auto save
(use-package super-save
  :diminish
  :defer 0.5
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (setq super-save-idle-duration 1)
  (setq super-save-auto-save-when-idle t)
  (setq save-silently t)
  (super-save-mode 1))


(provide 'init-misc)
;;; init-misc.el ends here
