;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flyspell
  :diminish
  :ensure nil
  :if (executable-find "aspell")
  ;; Add spell-checking in comments for all programming language modes
  :hook ((prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil
        ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=fast" "--lang=en_US" "--camel-case")
        ispell-personal-dictionary
        (expand-file-name "en_US.personal" "~/.config/aspell/")))

;; Correcting words with flyspell via completing-read
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-," . flyspell-correct-wrapper)))


(provide 'init-spelling)
;;; init-spelling.el ends here
