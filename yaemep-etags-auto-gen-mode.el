(require 'yaemep)


;;;###autoload
(define-minor-mode yaemep-etags-auto-gen-mode
  "Automatically create etags TAGS file for Erlang projects"
  :lighter " yaemep-etags")

;;;###autoload

(add-hook 'erlang-mode-hook 'yaemep-etags-auto-gen-mode)

(defun yaemep-etags-auto-gen-mode-toggle ()
  (if yaemep-etags-auto-gen-mode
      (progn
        (yaemep-project-etags-update-in-background)
        (add-hook 'after-save-hook 'yaemep-project-etags-update-in-background nil t))
    (progn
      (remove-hook 'after-save-hook 'yaemep-project-etags-update-in-background t))))

(add-hook 'yaemep-etags-auto-gen-mode-hook
          'yaemep-etags-auto-gen-mode-toggle)

(provide 'yaemep-etags-auto-gen-mode)
