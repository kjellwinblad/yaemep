(require 'yaemep)


(defun erlang-yaemep-help ()
  "Show YAEMEP help"
  (interactive)
  (with-output-to-temp-buffer "*YAEMEP Help*"
    (princ
     (with-temp-buffer
       (insert-file-contents
        (concat (file-name-as-directory (file-name-directory
                                         (locate-file "yaemep.el" load-path)))
                "README.md"))
       (buffer-string)))))

;;;###autoload
(define-minor-mode erlang-yaemep-extra-erlang-menu-mode
  "Add an extra Emacs menu with useful stuff"
  :lighter " erlang-yaemep-menu")

;;;###autoload

(add-hook 'erlang-mode-hook 'erlang-yaemep-extra-erlang-menu-mode)

(defun erlang-yaemep-extra-erlang-menu-mode-toggle ()
  (if erlang-yaemep-extra-erlang-menu-mode
      (progn
        ;; Creating a new menu pane in the menu bar to the right of “Tools” menu
        (define-key-after
          global-map
          [menu-bar yaemep-menu]
          (cons "Erlang YAEMEP" (make-sparse-keymap "???"))
          'tools )

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-complete]
          '("Complete Thing At Point" . erlang-yaemep-company-complete-or-completion-at-point))

        (define-key
          global-map
          [menu-bar yaemep-menu yamep-generate-etags]
          '("Generate TAGS for Project" . erlang-yaemep-project-etags-update))

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-help]
          '("Help" . erlang-yaemep-help)))
    (progn
      )))

(add-hook 'erlang-yaemep-extra-erlang-menu-mode-hook
          'erlang-yaemep-extra-erlang-menu-mode-toggle)

(provide 'erlang-yaemep-extra-erlang-menu-mode)
