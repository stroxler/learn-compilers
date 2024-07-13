; Copyright 2021 Jesse Haber-Kucharsky
; SPDX-License-Identifier: GPL-3.0-only

(defconst tiger-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; A string delimiter.
    (modify-syntax-entry ?\" "\"" table)
    ;; Division, and the start of a line comment.
    (modify-syntax-entry ?/ ". 12" table)
    ;; A newline ends a comment.
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst tiger-mode-lock-keywords
  (let* ((keywords '("let" "var" "in" "function" "type"
		     "while" "if" "else" "for" "array"
		     "of" "do" "end" "then" "and"
		     "size" "break"))
	 (keywords-regexp (regexp-opt keywords 'symbols)))
    `((,keywords-regexp . font-lock-keyword-face))))

(define-derived-mode tiger-mode
  prog-mode
  "Tiger"
  :syntax-table tiger-mode-syntax-table
  (setq font-lock-defaults '((tiger-mode-lock-keywords)))
  (font-lock-fontify-buffer)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(add-to-list 'auto-mode-alist '("\\.tig\\'" . tiger-mode))
(provide 'tiger-mode)

;;; tiger-mode.el ends here
