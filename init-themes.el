;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package color-theme-sanityinc-tomorrow
  :hook (after-init . reapply-themes)
  :bind ("C-c t b" . sanityinc-tomorrow-themes-toggle)
  :custom
  ;; Don't prompt to confirm theme safety. This avoids problems with
  ;; first-time startup on Emacs > 26.3.
  (custom-safe-themes t)
  ;; If you don't customize it, this is the theme you get.
  (custom-enabled-themes '(sanityinc-tomorrow-bright))
  :preface
  ;; Ensure that themes will be applied even if they have not been customized
  (defun reapply-themes ()
    "Forcibly load the themes listed in `custom-enabled-themes'."
    (dolist (theme custom-enabled-themes)
      (unless (custom-theme-p theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  ;; Toggle between light and dark
  (defun light ()
    "Activate a light color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-day))
    (reapply-themes))

  (defun dark ()
    "Activate a dark color theme."
    (interactive)
    (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
    (reapply-themes))

  (defun sanityinc-tomorrow-themes-toggle ()
    "Toggle between `sanityinc-tomorrow-bright' and `sanityinc-tomorrow-day'."
    (interactive)
    (if (eq (car custom-enabled-themes) 'sanityinc-tomorrow-bright)
        (light)
      (dark))))


(use-package dimmer
  :hook (after-init . dimmer-mode)
  :config
  (setq-default dimmer-fraction 0.15)
  (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all)))
  (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)
  :preface
  (defun sanityinc/display-non-graphic-p ()
    (not (display-graphic-p))))


(provide 'init-themes)
;;; init-themes.el ends here
