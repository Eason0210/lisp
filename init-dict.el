;;; init-dict.el --- Dictionaries support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when *is-a-mac*
  (use-package osx-dictionary
    :bind (("C-c t i" . osx-dictionary-search-input)
           ("C-c t x" . osx-dictionary-search-pointer))))

(use-package fanyi
  :bind (("C-c t f" . fanyi-dwim)
         ("C-c t d" . fanyi-dwim2))
  :config
  (setq fanyi-haici-chart-inhibit-same-window t)
  :custom
  (fanyi-providers '(fanyi-haici-provider
                     fanyi-youdao-thesaurus-provider
                     fanyi-etymon-provider
                     fanyi-longman-provider)))


(provide 'init-dict)
;;; init-dict.el ends here
