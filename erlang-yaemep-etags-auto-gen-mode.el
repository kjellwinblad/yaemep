(require 'yaemep)


;;;###autoload
(define-minor-mode erlang-yaemep-etags-auto-gen-mode
  "Automatically create etags TAGS file for Erlang projects"
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
