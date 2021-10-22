;;; init-rime.el --- Setup emacs-rime -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rime
  :if rime-usr-data-exists-p
  :bind (("C-\\" . toggle-input-method)
         ("C-`" . rime-send-keybinding)
         ([f8] . rime-toggle-show-candidate))
  :init
  (setq
   rime-inline-predicates '(rime-predicate-space-after-cc-p
                            rime-predicate-current-uppercase-letter-p)
   rime-translate-keybindings '("C-f" "C-b" "C-n" "C-p" "C-g")
   rime-inline-ascii-trigger 'shift-r
   rime-inline-ascii-holder ?a
   default-input-method "rime"
   rime-cursor "|"
   rime-show-candidate nil
   window-min-height 1
   rime-user-data-dir "~/emacs-data/rime"
   rime-title "")
  (when (eq system-type 'windows-nt)
    (setq rime-share-data-dir
          "~/scoop/apps/msys2/current/mingw64/share/rime-data"))
  (when *is-a-mac*
    (setq rime-librime-root  "~/emacs-data/librime/dist")
    (setq rime-emacs-module-header-root "~/.nix-profile/include"))
  :config
  ;; change cursor color automatically
  (use-package im-cursor-chg
    :ensure nil
    :after rime
    :config
    (cursor-chg-mode 1))
  :preface
  (defconst rime-usr-data-exists-p
    (file-exists-p "~/emacs-data/rime")
    "Checking if there is a rime user data.")

  (defun rime-toggle-show-candidate ()
    "Use minibuffer for candidate if current is nil."
    (interactive)
    (if (equal rime-show-candidate nil)
        (setq rime-show-candidate 'minibuffer)
      (setq rime-show-candidate nil))))


(provide 'init-rime)
;;; init-rime.el ends here
