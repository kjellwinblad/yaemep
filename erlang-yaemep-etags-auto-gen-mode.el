(require 'yaemep)

(defun erlang-yaemep-company-complete-or-completion-at-point ()
  "Run company-complete if activated and run completion-at-point
 otherwise"
  (interactive)
  (if (and (boundp 'company-mode) company-mode (fboundp 'company-complete))
      (company-complete)
    (completion-at-point)))


;;;###autoload
(define-minor-mode erlang-yaemep-etags-auto-gen-mode
  ""
  :lighter " erlang-yaemep-etags")

;;;###autoload

(add-hook 'erlang-mode-hook 'erlang-yaemep-etags-auto-gen-mode)

(defun erlang-yaemep-etags-auto-gen-mode-toggle ()
  (if erlang-yaemep-etags-auto-gen-mode
      (progn
        (erlang-yaemep-project-etags-update-in-background)
        (add-hook 'after-save-hook 'erlang-yaemep-project-etags-update-in-background nil t))
    (progn
      (remove-hook 'after-save-hook 'erlang-yaemep-project-etags-update-in-background t))))

(add-hook 'erlang-yaemep-etags-auto-gen-mode-hook
          'erlang-yaemep-etags-auto-gen-mode-toggle)

(provide 'erlang-yaemep-etags-auto-gen-mode)
