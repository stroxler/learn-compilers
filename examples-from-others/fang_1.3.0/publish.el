; Copyright 2021 Jesse Haber-Kucharsky
; SPDX-License-Identifier: GPL-3.0-only

(setq org-html-validation-link nil)

(defun publish (file-name)
  (interactive)
  (switch-to-buffer (find-file file-name))
  (org-html-export-to-html))
