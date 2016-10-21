;;; djinni-mode.el --- Major mode for Djinni IDL files

;; Copyright © 2016 Jason Foreman

;; Author: Jason Foreman <j@jafo.io>
;; Keywords: languages
;; Package-Requires: ((emacs "24.1"))

;;; Commentary:

;;; Code:

(defgroup djinni nil
  "Major mode for editing Djinni IDL files."
  :tag "Djinni"
  :group 'languages)

(defcustom djinni-indent-level 4
  "Number of spaces per indentation level."
  :type 'integer
  :group 'djinni
  :safe 'integerp)

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

(defun djinni-mode:indent-line ()
  "Indent the current line."
  (interactive)
  (let ((indent-level))
    (save-excursion
      ;; find the current delimiter nesting level
      (setq indent-level (nth 0 (syntax-ppss (point-at-bol))))
      ;; reduce level by 1 if we're at the closing delimiter
      (when (looking-at "\\s-*\\s)")
        (setq indent-level (1- indent-level))))
    (indent-line-to (* indent-level djinni-indent-level))))

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

  ;; indentation
  (setq-local indent-line-function #'djinni-mode:indent-line)
  (setq indent-tabs-mode nil)

  (setq font-lock-defaults '(djinni-mode:font-lock-defaults)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.djinni\\'" . djinni-mode))

(provide 'djinni-mode)

;;; djinni-mode.el ends here
