;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c x" . org-capture)
         :map org-mode-map
         ("C-c i a" . org-id-get-create)
         ("C-c e d" . org-export-docx))
  :config
  ;; To speed up startup, don't put to init section
  (setq org-hide-emphasis-markers t)

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((C . t)
     (calc . t)
     (dot . t)
     (emacs-lisp . t)
     (haskell . t)
     (python . t)
     (sql . t)
     (sqlite . t)))

  :preface
  ;; Export to docx
  (defun org-export-docx ()
    (interactive)
    (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
          (template-file (expand-file-name "template/template.docx"
                                           user-emacs-directory)))
      (shell-command (format "pandoc %s -o %s --reference-doc=%s"
                             (buffer-file-name)
                             docx-file
                             template-file))
      (message "Convert finish: %s" docx-file))))


;; Writing mode similar to the famous Writeroom editor for OS X
(use-package writeroom-mode
  :hook (org-mode . prose-mode)
  :preface
  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
    :init-value nil :lighter " Prose" :keymap nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq word-wrap-by-category t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'word-wrap-by-category)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0)))))


;; Roam
(when (and (executable-find "sqlite3") (executable-find "cc"))
  (use-package org-roam
    :diminish
    :bind (("C-c n a" . org-roam-db-autosync-mode)
           ("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n g" . org-roam-graph)
           ("C-c n i" . org-roam-node-insert)
           ("C-c n c" . org-roam-capture)
           ("C-c n j" . org-roam-dailies-capture-today)
           ("C-c n r" . org-roam-ref-find)
           ("C-c n s" . org-roam-db-sync))
    :init
    (setq org-roam-directory (file-truename "~/.org/org-roam")
          org-roam-db-location "~/.org/org-roam.db"
          org-roam-db-gc-threshold most-positive-fixnum
          org-roam-v2-ack t)
    :config
    (unless (file-exists-p org-roam-directory)
      (make-directory org-roam-directory t))

    (add-to-list 'display-buffer-alist
                 '("\\*org-roam\\*"
                   (display-buffer-in-direction)
                   (direction . right)
                   (window-width . 0.33)
                   (window-height . fit-window-to-buffer)))))


(provide 'init-org)
;;; init-org.el ends here
