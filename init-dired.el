;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq-default dired-kill-when-opening-new-dired-buffer t)
  (setq dired-recursive-copies 'always))

(use-package diredfl
  :config
  (diredfl-global-mode 1))


(provide 'init-dired)
;;; init-dired.el ends here
