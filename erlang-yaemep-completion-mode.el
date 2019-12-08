
;;https://nullprogram.com/blog/2013/02/06/


(require 'yaemep)


(defun erlang-yaemep-company-complete-or-completion-at-point ()
  "Run company-complete if activated and run completion-at-point
 otherwise"
  (interactive)
  (if (and (boundp 'company-mode) company-mode (fboundp 'company-complete))
      (company-complete)
    (completion-at-point)))


;;;###autoload
(define-minor-mode erlang-yaemep-completion-mode
  ""
  :lighter " erlang-yaemep-comp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-TAB") 'erlang-yaemep-company-complete-or-completion-at-point)
            map))

;;;###autoload

(add-hook 'erlang-mode-hook 'erlang-yaemep-completion-mode)

(defun erlang-yaemep-completion-mode-toggle ()
  (message "TOGGGLE")
  (if erlang-yaemep-completion-mode
      (progn
        (message "Starting erlang-yaemep-completion-mode")
        (add-hook 'completion-at-point-functions 'erlang-yaemep-completion-at-point nil t)
        (add-hook 'after-save-hook 'erlang-yaemep-completion-cache-update-in-background nil t)
        (erlang-yaemep-completion-cache-update-in-background))
    (progn
      (remove-hook 'completion-at-point-functions 'erlang-yaemep-completion-at-point t)
      (remove-hook 'after-save-hook 'erlang-yaemep-completion-cache-update-in-background t)
      (message "Exiting"))))

(add-hook 'erlang-yaemep-completion-mode-hook 'erlang-yaemep-completion-mode-toggle)

(provide 'erlang-yaemep-completion-mode)
