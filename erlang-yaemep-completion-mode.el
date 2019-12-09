
;;https://nullprogram.com/blog/2013/02/06/


(require 'yaemep)


(defun erlang-yaemep-company-complete-or-completion-at-point ()
  "Run company-complete if activated and run completion-at-point
 otherwise"
  (interactive)
  (if (and (boundp 'company-mode) company-mode (fboundp 'company-complete))
      (company-complete)
    (completion-at-point)))


(defun erlang-yaemep-completion ()
  (interactive)
  (message "A%sA" (and (boundp 'erlang-yaemep-completion-mode)
           erlang-yaemep-completion-mode))
  (if (and (boundp 'erlang-yaemep-completion-mode)
           erlang-yaemep-completion-mode)
      (erlang-yaemep-company-complete-or-completion-at-point)
    (with-output-to-temp-buffer "*YAEMEP erlang-yaemep-completion-mode not active*"
      (princ
       "

YAEMEP completion does not work when
erlang-yaemep-completion-mode is inactive. Please make sure that
erlang-yaemep-completion-mode is active (you should have the text
yaemep-comp just above the message and minibuffer area in your
Emacs window). YAMEP install instructions and documentation
should be available here:

https://github.com/kjellwinblad/yaemep"))))

;;;###autoload
(define-minor-mode erlang-yaemep-completion-mode
  "Add completion-at-point function for Erlang"
  :lighter " erlang-yaemep-comp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-TAB") 'erlang-yaemep-completion)
            map))

;;;###autoload

(add-hook 'erlang-mode-hook 'erlang-yaemep-completion-mode)

(defun erlang-yaemep-completion-mode-toggle ()
  (if erlang-yaemep-completion-mode
      (progn
        (add-hook 'completion-at-point-functions 'erlang-yaemep-completion-at-point nil t)
        (add-hook 'after-save-hook 'erlang-yaemep-completion-cache-update-in-background nil t)
        (erlang-yaemep-completion-cache-update-in-background))
    (progn
      (remove-hook 'completion-at-point-functions 'erlang-yaemep-completion-at-point t)
      (remove-hook 'after-save-hook 'erlang-yaemep-completion-cache-update-in-background t))))

(add-hook 'erlang-yaemep-completion-mode-hook 'erlang-yaemep-completion-mode-toggle)

(provide 'erlang-yaemep-completion-mode)
