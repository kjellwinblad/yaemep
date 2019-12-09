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


(defun erlang-yaemep-rebar3-compile-project ()
  (interactive)
  (compile (format "cd \"%s\" && rebar3 compile" (erlang-yaemep-project-dir))))

(defun erlang-yaemep-make-project ()
  (interactive)
  (compile (format "cd \"%s\" && make" (erlang-yaemep-project-dir))))



(defun erlang-yaemep-extra-erlang-menu-mode-toggle ()
  (if erlang-yaemep-extra-erlang-menu-mode
      (progn
        ;; Creating a new menu pane in the menu bar to the right of “Tools” menu
        (define-key-after
          global-map
          [menu-bar yaemep-menu]
          (cons "Erlang YAEMEP" (make-sparse-keymap "Erlang YAEMEP"))
          'tools )

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-rebar3-compile]
          '("Project: rebar3 compile" . erlang-yaemep-rebar3-compile-project))

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-rebar3-compile]
          '("Project: rebar3 compile" . erlang-yaemep-rebar3-compile-project))

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-make]
          '("Project: make" . erlang-yaemep-make-project))

        (define-key
          global-map
          [menu-bar yaemep-menu sep3]
          '(menu-item "--"))

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-normal-complete]
          '("Completion At Point" . completion-at-point))

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-complete]
          '("YAMEP Completion At Point" . erlang-yaemep-company-complete-or-completion-at-point))

        (define-key
          global-map
          [menu-bar yaemep-menu sep1]
          '(menu-item "--"))

        (define-key
          global-map
          [menu-bar yaemep-menu yamep-generate-etags]
          '("Generate TAGS for Project" . erlang-yaemep-project-etags-update))

        (define-key
          global-map
          [menu-bar yaemep-menu sep2]
          '(menu-item "--"))

        (define-key
          global-map
          [menu-bar yaemep-menu yaemep-help]
          '("Help" . erlang-yaemep-help)))
    (progn
      )))

(add-hook 'erlang-yaemep-extra-erlang-menu-mode-hook
          'erlang-yaemep-extra-erlang-menu-mode-toggle)

(provide 'erlang-yaemep-extra-erlang-menu-mode)
