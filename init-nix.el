;;; init-nix.el --- Support for the Nix package manager -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nixpkgs-fmt
  :after nix-mode
  :bind (:map nix-mode-map
              ("C-c C-f" . nixpkgs-fmt)))


(provide 'init-nix)
;;; init-nix.el ends here
