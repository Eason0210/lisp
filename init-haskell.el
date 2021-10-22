;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode
  :bind (:map haskell-mode-map
              ("C-c C-f" . ormolu-buffer))
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-auto-insert-module-template))

(use-package dante
  :after (haskell-mode flycheck)
  :hook (haskell-mode . dante-mode)
  :config
  (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint)))

(use-package reformatter
  :after haskell-mode
  :config
  (reformatter-define hindent
    :program "hindent"
    :lighter " Hin")

  (defalias 'hindent-mode 'hindent-on-save-mode)

  (reformatter-define ormolu
    :program "ormolu"
    :lighter " Orm"))


(provide 'init-haskell)
;;; init-haskell.el ends here
