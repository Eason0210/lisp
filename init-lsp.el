;;; init-lsp.el --- Support for Languages Server Protocol  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :defer t
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l f" . eglot-format)
              ("C-c l d" . eldoc))
  :hook (eglot-managed-mode . (lambda () (flymake-mode -1)))
  :config
  (add-to-list 'eglot-server-programs '(rust-mode "rust-analyzer"))
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

  (setq read-process-output-max (* 1024 1024))
  (push :documentHighlightProvider eglot-ignored-server-capabilities)
  (setq eldoc-echo-area-use-multiline-p nil))



(provide 'init-lsp)
;;; init-lsp.el ends here
