
YAEMEP -- Yet Another Emacs erlang-mode Extentions Package
==========================================================

YAEMEP is an Emacs package that contains extensions to the major mode
for Emacs called erlang-mode. YAEMEP is designed to work without
requiring the user to do any project specific set up and its only
dependencies are that erlang-mode is installed, and the escript
program (escript is included in standard Erlang/OTP
installations). One can easily select to use only a subset of the
extensions provided by YAEMEP. YAEMEP constains the following
extensions to erlang-mode:

* Autocompletion of module names, function names in a module, module
  local functions and local variables
* Automatic generation of etags for Erlang projects (makes it posible
  to, e.g., go to the function under the cursor by pressing "M-.")

Quick Install
-------------

1. Make sure erlang-mode is installed:
   https://erlang.org/doc/apps/tools/erlang_mode_chapter.html
2. Download YAEMEP (e.g., by running `git clone https://github.com/kjellwinblad/yaemep.git`)
3. Put the following in your Emacs init file:

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; !!!IMPORTANT!!! Add yaemep to your Emacs load-path
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (add-to-list
        'load-path
        (or (and (boundp 'erlang-mode-path) erlang-mode-path)
        "/the/path/to/your/yaemep/"))
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;
        ;
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Install erlang-yaemep-completion-mode
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (require 'erlang-yaemep-completion-mode)
        ;; (Completion command can be invoked with "M-TAB")
        ;;
        ;; --------------------------------------------------------
        ;; (Optional but Recommended) Load company-mode when erlang- mode has
        ;; loaded if company mode is installed (will give you fancy in buffer
        ;; completions):
        ;; --------------------------------------------------------
        (if (fboundp 'company-mode)
        (add-hook 'erlang-mode-hook 'company-mode))
        ;;
        ;; --------------------------------------------------------
        ;; (Optional But Recomended) Install and load company-mode
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
        ;
        ;
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
4. Restart Emacs

User Guide
----------

### Completion

Press the shortcut for completion ("M-TAB" if you followed the install
guide above) somewhere inside an Erlang function definition to trigger
completion. The line the cursor is on need to start with a space or a
tab character for completion to work. Below is a list of special
completions:

* **modules, local functions and variables** Press the completion
  shortcut when there is a space before the cursor/point
* **Only modules** Press the completion when the cursor is just before
  a coloun character (i.e., `:`).
* **Only local functions** Press the completion key when the
  cursor/point is just before the string "()".
* **Only variables** Press the completion key when the cursor/point


Only Install the `completion-at-point` Function `yaep-erlang-completion-at-point`
-----------------------------------------------------------------------------

The yaep-erlang-completion-at-point function that YAEMEP provides can
be hooked into the completion-at-point framework that is included in
Emacs. This will make it posible to get listings and completions of
module names, function names in a module, module local functions and
local variables. Add the following lines to your Emacs init file 




Other Similar Projects
----------------------
