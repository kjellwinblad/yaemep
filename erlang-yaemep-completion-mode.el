
;;https://nullprogram.com/blog/2013/02/06/


(require 'yaemep)


;;;###autoload
(define-minor-mode erlang-yaemep-completion-mode
  ""
  :lighter " erlang-yaemep-comp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-TAB") 'completion-at-point)
            map))

;;;###autoload

(add-hook 'erlang-mode-hook 'erlang-yaemep-completion-mode)

(defun erlang-yaemep-completion-mode-toggle ()
  (message "TOGGGLE")
  (if erlang-yaemep-completion-mode
      (progn
        (message "Starting erlang-yaemep-completion-mode")
        (add-hook 'completion-at-point-functions 'erlang-yaemep-completion-at-point nil t))
    (progn
      (message "Exiting"))))

(add-hook 'erlang-yaemep-completion-mode-hook
          'erlang-yaemep-completion-mode-toggle)

(provide 'erlang-yaemep-completion-mode)
