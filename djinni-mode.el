;;; djinni-mode.el --- Major mode for Djinni IDL files

;; Copyright © 2016 Jason Foreman

;; Author: Jason Foreman <j@jafo.io>
;; Keywords: languages
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;;; Code:

(defconst djinni-mode:syntax-table
  (let ((table (make-syntax-table)))

    ;; djinni strings use double quotes
    (modify-syntax-entry ?\" "\"" table)

    ;; djinni uses bash-style comments
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)

    table))

(defconst djinni-mode:font-lock-defaults
  '(
    ("\\<[Tt]rue\\>" . font-lock-constant-face)
    ("\\<[Ff]alse\\>" . font-lock-constant-face)

    ("@import\\>" . font-lock-preprocessor-face)
    ("@extern\\>" . font-lock-preprocessor-face)

    ("\\<const\\>" . font-lock-keyword-face)
    ("\\<deriving\\>" . font-lock-keyword-face)
    ("\\<enum\\>" . font-lock-keyword-face)
    ("\\<interface\\>" . font-lock-keyword-face)
    ("\\<record\\>" . font-lock-keyword-face)
    ("\\<static\\>" . font-lock-keyword-face)
    ))

;;;###autoload
(define-derived-mode djinni-mode prog-mode "Djinni"
  :syntax-table djinni-mode:syntax-table

  ;; comments
  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local comment-end "")
  (setq-local comment-padding " ")
  (setq-local comment-use-syntax t)
  (setq-local comment-auto-fill-only-comments t)

  (setq font-lock-defaults '(djinni-mode:font-lock-defaults)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.djinni\\'" . djinni-mode))

(provide 'djinni-mode)

;;; djinni-mode.el ends here
