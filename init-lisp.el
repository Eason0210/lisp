;;; init-lisp.el --- Emacs lisp settings, and common config for other lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package scratch :defer t)

;; Make C-x C-e run 'eval-region if the region is active

(use-package lisp-mode
  :ensure nil
  :bind (([remap eval-expression] . pp-eval-expression)
         :map emacs-lisp-mode-map
         ("C-x C-e" . sanityinc/eval-last-sexp-or-region)
         ("C-c C-e" . pp-eval-expression)
         ("C-c C-l" . sanityinc/load-this-file))
  :hook ((emacs-lisp-mode . (lambda () (setq mode-name "ELisp")))
         (emacs-lisp-mode . sanityinc/maybe-set-bundled-elisp-readonly))
  :config
  (setq-default debugger-bury-or-kill 'kill)
  (setq-default initial-scratch-message
                (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

  ;; Load .el if newer than corresponding .elc

  (setq load-prefer-newer t)

  ;; Make C-x C-e run 'eval-region if the region is active
  (defun sanityinc/eval-last-sexp-or-region (prefix)
    "Eval region from BEG to END if active, otherwise the last sexp."
    (interactive "P")
    (if (and (mark) (use-region-p))
        (eval-region (min (point) (mark)) (max (point) (mark)))
      (pp-eval-last-sexp prefix)))

  (defun sanityinc/make-read-only (expression out-buffer-name)
    "Enable `view-mode' in the output buffer - if any - so it can be closed with `\"q\"."
    (when (get-buffer out-buffer-name)
      (with-current-buffer out-buffer-name
        (view-mode 1))))
  (advice-add 'pp-display-expression :after 'sanityinc/make-read-only)

  ;; C-c C-l to load buffer or file
  (defun sanityinc/load-this-file ()
    "Load the current file or buffer.
The current directory is temporarily added to `load-path'.  When
there is no current file, eval the current buffer."
    (interactive)
    (let ((load-path (cons default-directory load-path))
          (file (buffer-file-name)))
      (if file
          (progn
            (save-some-buffers nil (apply-partially 'derived-mode-p 'emacs-lisp-mode))
            (load-file (buffer-file-name))
            (message "Loaded %s" file))
        (eval-buffer)
        (message "Evaluated %s" (current-buffer)))))

  (defun sanityinc/maybe-set-bundled-elisp-readonly ()
    "If this elisp appears to be part of Emacs, then disallow editing."
    (when (and (buffer-file-name)
               (string-match-p "\\.el\\.gz\\'" (buffer-file-name)))
      (setq buffer-read-only t)
      (view-mode 1)))

  ;;respawn the scratch buffer when it's killed
  (use-package immortal-scratch
    :after lisp-mode
    :hook (after-init . immortal-scratch-mode))

  ;; Extras for theme editing
  (use-package highlight-quoted
    :hook (emacs-lisp-mode . highlight-quoted-mode)))

(use-package rainbow-mode
  :diminish
  :hook ((emacs-lisp-mode . sanityinc/enable-rainbow-mode-if-theme)
         (help-mode . rainbow-mode))
  :preface
  (defun sanityinc/enable-rainbow-mode-if-theme ()
    (when (and (buffer-file-name) (string-match-p "\\(color-theme-\\|-theme\\.el\\)" (buffer-file-name)))
      (rainbow-mode))))

(use-package macrostep
  :bind ("C-c e m" . macrostep-expand))

(use-package ert
  :ensure nil
  :bind (:map ert-results-mode-map
              ("g" . ert-results-rerun-all-tests)))

(use-package flycheck-package
  :after (flycheck elisp-mode)
  :config
  (flycheck-package-setup))

(use-package flycheck-relint
  :defer t
  :after (flycheck elisp-mode))


;; Enable desired features for all lisp modes

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun sanityinc/enable-check-parens-on-save ()
  "Run `check-parens' when the current buffer is saved."
  (add-hook 'after-save-hook #'check-parens nil t))

(defvar sanityinc/lispy-modes-hook
  '(enable-paredit-mode
    sanityinc/enable-check-parens-on-save)
  "Hook run in all Lisp modes.")

(use-package aggressive-indent
  :config
  (add-to-list 'sanityinc/lispy-modes-hook 'aggressive-indent-mode))

(defun sanityinc/lisp-setup ()
  "Enable features useful in any Lisp mode."
  (run-hooks 'sanityinc/lispy-modes-hook))

(defun sanityinc/emacs-lisp-setup ()
  "Enable features useful when working with elisp."
  (set-up-hippie-expand-for-elisp))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
          '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

(when (boundp 'eval-expression-minibuffer-setup-hook)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


(provide 'init-lisp)
;;; init-lisp.el ends here
