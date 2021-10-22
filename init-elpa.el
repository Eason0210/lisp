;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; Standard package repositories
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(require 'package)
(package-initialize)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-compute-statistics t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding
;; the required packages to package-selected-packages after startup is complete.
;; Make `package-autoremove' work with `use-package'

(defvar use-package-selected-packages '(use-package)
  "Packages pulled in by use-package.")

(eval-and-compile
  (define-advice use-package-handler/:ensure (:around (fn name-symbol keyword args rest state) select)
    (let ((items (funcall fn name-symbol keyword args rest state)))
      (dolist (ensure args items)
        (let ((package
               (or (and (eq ensure t) (use-package-as-symbol name-symbol))
                   ensure)))
          (when package
            (when (consp package)
              (setq package (car package)))
            (push `(add-to-list 'use-package-selected-packages ',package) items)))))))

(when (fboundp 'package--save-selected-packages)
  (add-hook 'after-init-hook
            (lambda ()
              (package--save-selected-packages
               (seq-uniq (append use-package-selected-packages package-selected-packages))))))


;; Auto update packages
(use-package auto-package-update
  :init
  (setq auto-package-update-delete-old-versions t
        auto-package-update-hide-results t)
  (defalias 'upgrade-packages #'auto-package-update-now))


(use-package diminish)

(use-package bind-key
  :bind ("C-h y" . describe-personal-keybindings))


(provide 'init-elpa)
;;; init-elpa.el ends here
