;;; init-whitespace.el --- Special handling for whitespace -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/show-trailing-whitespace ()
  "Enable display of trailing whitespace in this buffer."
  (setq-local show-trailing-whitespace t))

(dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook hook 'sanityinc/show-trailing-whitespace))

(bind-key [remap just-one-space] 'cycle-spacing)


(provide 'init-whitespace)
;;; init-whitespace.el ends here