;;; init-hippie-expand.el --- Settings for hippie-expand -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill)))


(provide 'init-hippie-expand)
;;; init-hippie-expand.el ends here
