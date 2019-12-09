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

(defun erlang-yaemep-menu-error-info ()
  "Explain why YAMEP does not work"
  (interactive)
    (with-output-to-temp-buffer "*YAEMEP Error! why?*"
    (princ
     (format "

YAEMEP will not work because the following command cannot run
correctly (it should print \"OK\"):

escript %s check

The reason for this is probably that you do not have the escript
program in any of your search paths for programs. Please make
sure that escript program is installed and that the directory
where it is located is in your search path variable. The escript
program is typically installed together with Erlang."
             (erlang-yaemep-get-support-escript-path)))))


(defun erlang-yaemep-doc-error-info ()
  "Explain why goto documentation does not work"
  (interactive)
    (with-output-to-temp-buffer "*YAEMEP Erlang Doc Broken*"
    (princ
     (format "

This function is broken in your version of Emacs
erlang-mode. Please upgrade to version 2.8.3 (20191023.843) or
later:

https://melpa.org/#/erlang"
             (erlang-yaemep-get-support-escript-path)))))

;;;###autoload
(define-minor-mode erlang-yaemep-extra-erlang-menu-mode
  "Add an extra Emacs menu with useful stuff"
  :lighter " erlang-yaemep-menu"
  :keymap (let ((map (make-sparse-keymap)))
            map))

;;;###autoload

(add-hook 'erlang-mode-hook 'erlang-yaemep-extra-erlang-menu-mode)


(defun erlang-yaemep-rebar3-compile-project ()
  (interactive)
  (compile (format "cd \"%s\" && rebar3 compile" (erlang-yaemep-project-dir))))

(defun erlang-yaemep-make-project ()
  (interactive)
  (compile (format "cd \"%s\" && make" (erlang-yaemep-project-dir))))

(defun erlang-yaemep-mix-compile-project ()
  (interactive)
  (compile (format "cd \"%s\" && mix compile" (erlang-yaemep-project-dir))))


(defun erlang-yaemep-completion-from-menu ()
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


(defun erlang-yaemep-extra-erlang-menu-mode-toggle ()
  (if erlang-yaemep-extra-erlang-menu-mode
      (progn
        ;; Creating a new menu pane in the menu bar to the right of “Tools” menu
        (define-key-after
          erlang-yaemep-extra-erlang-menu-mode-map
          [menu-bar yaemep-menu]
          (cons "Erlang YAEMEP" (make-sparse-keymap "Erlang YAEMEP"))
          'tools )

        (if (erlang-yaemep-check-support-escript "the yamep menu")
            (progn
              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu yaemep-mix-compile]
                '("Project: mix compile" . erlang-yaemep-mix-compile-project))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu yaemep-rebar3-compile]
                '("Project: rebar3 compile" . erlang-yaemep-rebar3-compile-project))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu yaemep-make]
                '("Project: make" . erlang-yaemep-make-project))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu sep5]
                '(menu-item "--"))

              (if (boundp 'erlang-version)
                  (if (string< erlang-version "2.8.3")
                      (define-key
                        erlang-yaemep-extra-erlang-menu-mode-map
                        [menu-bar yaemep-menu yaemep-goto-erlang-man]
                        '("Documentation for Erlang/OTP Function Under Point" . erlang-yaemep-doc-error-info))
                    (define-key
                      erlang-yaemep-extra-erlang-menu-mode-map
                      [menu-bar yaemep-menu yaemep-goto-erlang-man]
                      '("Documentation for Erlang/OTP Function Under Point" . erlang-man-function-no-prompt))))

              (if (and (fboundp 'xref-find-definitions)
                       (fboundp 'xref-pop-marker-stack))
                  (progn
                    (define-key
                      erlang-yaemep-extra-erlang-menu-mode-map
                      [menu-bar yaemep-menu sep4]
                      '(menu-item "--"))

                    (define-key
                      erlang-yaemep-extra-erlang-menu-mode-map
                      [menu-bar yaemep-menu yaemep-goto-thing-at-point]
                      '("Go Back After Go to Thing at Point" . xref-pop-marker-stack))

                    (define-key
                      erlang-yaemep-extra-erlang-menu-mode-map
                      [menu-bar yaemep-menu yaemep-goto-thing-at-point-go-back]
                      '("Go to Thing at Point" . xref-find-definitions))))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu sep3]
                '(menu-item "--"))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu yaemep-complete]
                '("Completion At Point" . erlang-yaemep-completion))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu sep1]
                '(menu-item "--"))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu yamep-generate-etags]
                '("Generate TAGS for Project" . erlang-yaemep-project-etags-update))

              (define-key
                erlang-yaemep-extra-erlang-menu-mode-map
                [menu-bar yaemep-menu sep2]
                '(menu-item "--")))
          (progn
            (define-key
              erlang-yaemep-extra-erlang-menu-mode-map
              [menu-bar yaemep-menu yaemep-menu-error-why]
              '("Error! Why?" . erlang-yaemep-menu-error-info))))


        (define-key
          erlang-yaemep-extra-erlang-menu-mode-map
          [menu-bar yaemep-menu yaemep-help]
          '("Help" . erlang-yaemep-help)))
    (progn
      )))

(add-hook 'erlang-yaemep-extra-erlang-menu-mode-hook
          'erlang-yaemep-extra-erlang-menu-mode-toggle)

(provide 'erlang-yaemep-extra-erlang-menu-mode)
