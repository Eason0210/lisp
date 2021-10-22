;;; init-j.el --- Basic support for programming in J -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package j-mode
  :defer t
  :hook (inferior-j-mode . (lambda () (electric-pair-mode -1)))
  :config
  (setq-default j-console-cmd "jconsole"))


(provide 'init-j)
;;; init-j.el ends here
