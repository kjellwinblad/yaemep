;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following block of lines should not be included in your own
;; Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq erlang-mode-path (concat user-emacs-directory "erlang"))
(setq yaemep-path
      (file-name-directory
       (directory-file-name
	(file-name-directory
	 (directory-file-name user-emacs-directory)))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; !!!IMPORTANT!!! Add yaemep to your Emacs load-path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list
 'load-path
 (or (and (boundp 'yaemep-path) yaemep-path)
     "/the/path/to/your/yaemep/"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install erlang-yaemep-completion-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'erlang-yaemep-completion-mode)
;; (Completion command can be invoked with "M-TAB")
;;
;; --------------------------------------------------------
;;(Optional but Recommended) Load company-mode when erlang-mode has
;; loaded (will give you fancy in-buffer completions if company-mode
;; is installed):
;; --------------------------------------------------------
(add-hook 'erlang-mode-hook 'company-mode)
;; --------------------------------------------------------
;;
;; --------------------------------------------------------
;; (Optional But Recomended) Install company-mode
;; --------------------------------------------------------
;;
;; Not necessary if you have installed company-mode on Ubunu or Debian
;; with:
;;
;; sudo apt-get install elpa-company
;;
(progn
  (require 'package)
  (package-initialize)
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))
  (unless (package-installed-p 'company)
    (package-install 'company)))
;; --------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install erlang-yaemep-etags-auto-gen-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'erlang-yaemep-etags-auto-gen-mode)
;; (Use "M-." to go to thing at point and "M-, to go back")
;;
;; Use the following variable to add extra directories to include in
;; the TAGS file for the project. Erlang/OTP source code directories
;; in this list will be ignored if the project directory is an
;; Erlang/OTP source code directory. Non-existing directories will
;; also be ignored. The directory "release/tests" will be ignored
;; inside Erlang/OTP source code directories.
(setq erlang-yaemep-etags-auto-gen-extra-dirs (list "/home/user/src/otp/"))
;; Change the following variable if you want to include other types of
;; files in your TAGS file.
(setq erlang-yaemep-etags-auto-gen-search-pattern "**/*.{erl,hrl}")
;; You may want to activate erlang-yaemep-etags-auto-gen-mode in other
;; major modes if you changed the
;; erlang-yaemep-etags-auto-gen-search-pattern variable above.
;;
;; For example:
;;
;;(add-hook 'c-mode 'erlang-yaemep-etags-auto-gen-mode)
;;
;; -------------------------------------------------------
;; (Optional but Recomended) Load etags file automatically after it
;; has got updated
;; -------------------------------------------------------
;;
;; The etags-table.el is in the yaemep folder for your convenience.
;;
;; See https://www.emacswiki.org/emacs/EtagsTable for more information
;; about etags-table
;;
;; -------------------------------------------------------
(setq tags-revert-without-query 1)
(add-to-list 'load-path
             (concat (file-name-directory
                       (locate-file "yaemep.el" load-path))
                     "etags-table"))
(require 'etags-table)
(setq etags-table-search-up-depth 99)
;; -------------------------------------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


