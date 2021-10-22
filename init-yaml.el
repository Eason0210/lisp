;;; init-yaml.el --- Support Yaml files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . goto-address-prog-mode))


(provide 'init-yaml)
;;; init-yaml.el ends here
