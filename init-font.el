;;; init-font.el --- Configure fonts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar font-list
  (cond
   ((eq system-type 'darwin)
    '(("SF Mono" . 130) ("Monaco" . 130) ("Menlo" . 130)))
   ((eq system-type 'windows-nt)
    '(("SF Mono" . 110) ("Consolas" . 120) ("Cascadia Mono" . 110)))
   (t
    '(("SF Mono" . 110) ("Consolas" . 120) ("Cascadia Mono" . 110))))
  "List of fonts and sizes.  The first one available will be used.")

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun change-font ()
  "Set English font from the `font-list'."
  (interactive)
  (let* (available-fonts font-name font-size)
    (dolist (font font-list
                  (setq available-fonts (nreverse available-fonts)))
      (when (font-installed-p (car font))
        (push font available-fonts)))
    (if (not available-fonts)
        (message "No fonts from the chosen set are available")
      (if (called-interactively-p 'interactive)
          (let* ((chosen (assoc-string
                          (completing-read "What font to use? "
                                           available-fonts nil t)
                          available-fonts)))
            (setq font-name (car chosen)
                  font-size (read-number "Font size: " (cdr chosen))))
        (setq font-name (caar available-fonts)
              font-size (cdar available-fonts)))
      (set-face-attribute 'default nil :font font-name :height font-size))))

(when (display-graphic-p)
  (change-font)

  (dolist (font '("Segoe UI Symbol" "Apple Color Emoji" "Noto Color Emoji"))
    (if (font-installed-p font)
        (set-fontset-font t 'unicode font nil 'prepend)))

  (dolist (font '("Microsoft Yahei" "Hiragino Sans GB" "Noto Sans Mono CJK SC"))
    (if (font-installed-p font)
        (set-fontset-font t '(#x4e00 . #x9fff) font))))


(provide 'init-font)
;;; init-font.el ends here
