;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

(use-package magit
  :bind (("C-x g" . magit-status)))


(provide 'init-git)
;;; init-git.el ends here
