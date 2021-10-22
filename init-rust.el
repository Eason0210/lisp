;;; init-rust.el --- Support for the Rust language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :bind (:map rustic-mode-map
              ("C-c C-f" . rustic-format-buffer))
  :config
  (setq rustic-lsp-client 'eglot)
  (with-eval-after-load 'flycheck
    (push 'rustic-clippy flycheck-checkers)))


(provide 'init-rust)
;;; init-rust.el ends here
