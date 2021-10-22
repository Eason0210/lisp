;;; init-vc.el --- Version control support -*- lexical-binding: t -*-
;;; Commentary:

;; Most version control packages are configured separately: see
;; init-git.el, for example.

;;; Code:

(use-package diff-hl
  :bind (:map diff-hl-mode
              ("<left-fringe> <mouse-1>" . diff-hl-diff-goto-hunk))
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
  (after-init . global-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode))


(provide 'init-vc)
;;; init-vc.el ends here
