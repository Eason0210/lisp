;;; init-compile.el --- Helpers for M-x compile -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package compile
  :ensure nil
  :bind ([f6] . recompile)
  :config
  (setq-default compilation-scroll-output t))

(use-package quickrun
  :bind (("<f5>" . quickrun)
         ("C-<f5>" . quickrun-shell))
  :config
  (quickrun-add-command "c++/c1z"
    '((:command . "g++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
		   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))


(provide 'init-compile)
;;; init-compile.el ends here
