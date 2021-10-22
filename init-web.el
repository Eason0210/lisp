;;; init-web.el --- Web configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; SASS and SCSS mode
;; (use-package sass-mode :defer t)
(setq-default scss-compile-at-save nil)

;; LESS
(use-package skewer-less
  :hook (less-css-mode . skewer-less-mode))

;; JavaScript
(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq-default js-indent-level 2)
  :config
  (add-to-list 'flycheck-disabled-checkers #'javascript-jshint)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (with-eval-after-load 'js2-mode
    (sanityinc/major-mode-lighter 'js2-mode "JS2")
    (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2")))


;; JSON mode
(use-package json-mode
  :mode "\\.json\\'")

(use-package json-reformat
  :bind (:map json-mode-map
              ("C-c C-f" . json-reformat-region))
  :after json-mode)

(use-package json-snatcher
  :after json-mode)

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish
  :hook (((js-mode js2-mode). skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode)))

(use-package typescript-mode
  :mode ("\\.ts[x]\\'" . typescript-mode))

;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))


(provide 'init-web)
;;; init-web.el ends here
