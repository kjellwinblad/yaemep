;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following block of lines should not be included in your own
;; Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq erlang-mode-path (concat user-emacs-directory "erlang"))
(setq yaemep-path
      (or (and (file-directory-p "/the/path/to/your/yaemep/") "/the/path/to/your/yaemep/")
      (file-name-directory
       (directory-file-name
	(file-name-directory
	 (directory-file-name user-emacs-directory))))))
;; Could be unsafe but fixes experied key issue
(setq package-check-signature nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Emacs Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 2)
(require 'ido)
(ido-mode 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install Emacs Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list
 'load-path
 (or (and (boundp 'erlang-mode-path) erlang-mode-path)
     "/the/path/to/your/erlang-mode/directory"))
(require 'erlang-start)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; (The line below needs to be changed!) Add YAMEP to your Emacs load-path
(add-to-list 'load-path (or (and (boundp 'yaemep-path) yaemep-path) "/the/path/to/your/yaemep/"))

;; Install yaemep-completion-mode
;; (Completion command can be invoked with "M-TAB")
(require 'yaemep-completion-mode)

;; Install yaemep-etags-auto-gen-mode
;; (Use "M-." to go to thing at point and "M-," to go back")
(require 'yaemep-etags-auto-gen-mode)

;; Install yaemep-extra-erlang-menu-mode
(require 'yaemep-extra-erlang-menu-mode)



;; (Optional) Configure yaemep-etags-auto-gen-mode
;;
;; Use the following variable to add extra directories to include in
;; the TAGS file for your project. Erlang/OTP source code directories
;; in this list will be ignored if the project directory is an
;; Erlang/OTP source code directory. Non-existing directories will
;; also be ignored. The directory "release/tests" will be ignored
;; inside Erlang/OTP source code directories.

(setq yaemep-etags-auto-gen-extra-dirs (list "/home/user/src/otp/"))

;; Change the following variable if you want to include other types of
;; files in your TAGS file.

(setq yaemep-etags-auto-gen-search-pattern "**/*.{erl,hrl}")

;; You may want to activate yaemep-etags-auto-gen-mode in other
;; major modes if you changed the
;; yaemep-etags-auto-gen-search-pattern variable above.
;;
;; For example:
;;
;; (add-hook 'c-mode 'yaemep-etags-auto-gen-mode)

;; (Optional) Load etags file automatically after it
;; has got updated:
;;
;; The etags-table/etags-table.el is in the yaemep folder for your
;; convenience.
;;
;; See https://www.emacswiki.org/emacs/EtagsTable for more information
;; about etags-table
;;
(setq tags-revert-without-query 1)
(add-to-list 'load-path
             (concat (file-name-directory
                       (locate-file "yaemep.el" load-path))
                     "etags-table"))
(require 'etags-table)
(setq etags-table-search-up-depth 99)



;; --------------------------------------------------------
;; (Optional But Recomended) Install company-mode
;; --------------------------------------------------------
;;
;; Not necessary if you have installed company-mode on Ubunu or Debian
;; with:
;;
;; sudo apt-get install elpa-company
;;

(require 'package)
(package-initialize)
(unless (package-installed-p 'company)
  (progn
    (or (file-exists-p package-user-dir)
        (package-refresh-contents))
    (package-install 'company)))

;; Load company-mode when erlang-mode has
;; loaded (will give you fancy in-buffer completions if company-mode
;; is installed):

(add-hook 'erlang-mode-hook 'company-mode)
;; --------------------------------------------------------


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-common ((((type x)) (:inherit company-tooltip :weight bold)) (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection ((((type x)) (:inherit company-tooltip-selection :weight bold)) (t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:background "steelblue" :foreground "white")))))
