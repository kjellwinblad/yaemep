
YAEMEP -- Yet Another Emacs Erlang Mode Extensions Package
==========================================================

YAEMEP is an Emacs package that contains extensions to the major mode
for Emacs called
[erlang-mode](https://erlang.org/doc/apps/tools/erlang_mode_chapter.html). YAEMEP
is designed to work without requiring the user to do any project
specific set up and its only dependencies are that erlang-mode and the
escript program are installed (escript is included in standard
Erlang/OTP installations). One can easily select to use only a subset
of the extensions provided by YAEMEP. YAEMEP contains the following
extensions:

* **yaemep-completion-mode** Autocompletion of module names, function
  names in a module, module local functions and local variables
* **yaemep-etags-auto-gen-mode** Automatic generation of etags for
  Erlang projects (makes it possible to, e.g., go to the function
  under the cursor by pressing "M-.")
* **yaemep-extra-erlang-menu-mode** Adds a menu with shortcuts to
  useful YAMEP and Erlang/OTP functions.

Install
-------

1. Make sure that your system has a working `escript` program
   installed (see the [Erlang/OTP installation
   instructions](http://erlang.org/doc/installation_guide/INSTALL.html))
2. Make sure erlang-mode is installed correctly:
   https://erlang.org/doc/apps/tools/erlang_mode_chapter.html
3. Download YAEMEP (e.g., by running `git clone https://github.com/kjellwinblad/yaemep.git`)
4. Put the following in your [Emacs init
   file](https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html)
   (note that "/the/path/to/your/yaemep/" needs to be replaced with
   the correct path):

   ```elisp
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; !!!IMPORTANT: Needs change!!! Add yaemep to your Emacs load-path
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (add-to-list
    'load-path
        "/the/path/to/your/yaemep/")
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Install yaemep-completion-mode
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (require 'yaemep-completion-mode)
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
   ;; Install yaemep-etags-auto-gen-mode
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (require 'yaemep-etags-auto-gen-mode)
   ;; (Use "M-." to go to thing at point and "M-," to go back")
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
   ;;(add-hook 'c-mode 'yaemep-etags-auto-gen-mode)
   ;;
   ;; -------------------------------------------------------
   ;; (Optional but Recommended) Load etags file automatically after it
   ;; has got updated
   ;; -------------------------------------------------------
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
   ;; -------------------------------------------------------
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Install yaemep-extra-erlang-menu-mode
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   (require 'yaemep-extra-erlang-menu-mode)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ```
5. Restart Emacs


Test YAEMEP in Emacs Without Installing
---------------------------------------

Run the following commands in a command prompt:

```bash
git clone https://github.com/kjellwinblad/yaemep.git
./yaemep/example/emacs_with_erlang_and_yaemep.sh ~/my_fav_proj/my_fav_file.erl
```


User Guide
----------

### Completion with `yaemep-completion-mode`

Press the shortcut for completion somewhere inside an Erlang function
definition to trigger completion. The shortcut for completion is
"M-TAB" (i.e., the ALT-key and the TAB-key at the same time on most
systems). The line that the cursor/point is on need to be inside an
Erlang function and start with a space or a tab character for
completion to work. Below is a list of the types of completions
available and descriptions of how to trigger them:

* **modules, local functions and variables** -- Press the completion
  shortcut when there is a space before the cursor/point
* **Only modules** -- Press the completion when the cursor is just before
  a colon character (i.e., `:`).
* **Only local functions** -- Press the completion key when the
  cursor/point is just before the string "()".
* **Only variables** -- Press the completion key when the cursor/point
  is directly after an `@` character

### Go to Function at Point etc with `yaemep-etags-auto-gen-mode`

The Emacs minor-mode `yaemep-etags-auto-gen-mode` automatically
generates a TAG file for Erlang projects that allows you to go the
project function under point among other things. Only .erl and .hrl
files in the current project will be included in the TAGS file by
default but this can be changed by setting the variables
`yaemep-etags-auto-gen-extra-dirs`
`yaemep-etags-auto-gen-search-pattern`. See the install instructions
in the Install section above for details about how to set these
variables. Here is a list of useful shortcuts:

* **C-.** Go to function under point
* **C-,** Go back to original place after you have executed "go to
  function under point"

### Go to Function at Point etc with `yaemep-etags-auto-gen-mode`

The Emacs minor-mode `yaemep-etags-auto-gen-mode` will add a menu to
the top menu bar in Emacs with the title "Erlang YAMEP". This menu
contains useful shortcuts and key-combinations that can be used to
trigger them.

### How YAMEP Finds the Project Root

YAMEP's functions attempt to automatically find the root of the
project for the file that the current buffer is associated
with. YAMEP should be able to locate the root of most types of Erlang
projects. To find the project root YAMEP first tries to find a config
file for one of the build systems rebar3, mix and erlang.mk. If the
root of the project can't be found based on the build system, YAMEP
will look for a .git, .svn or .cvs folder (version control system
folders). If YAMEP cannot locate the project root using the methods
described above, the folder where the file that is associated with the
current buffer is stored will be used as the project root. Finally,
you can force YAMEP to use a particular folder by placing a file
called ".emacs_erlang_mode_project" in the root of your project. You
can type \"M-x yaemep-project-dir\" to check which directory YAMEP
will use as the project directory.

License
-------

YAEMEP is licensed under [Apache License Version 2.0](LICENSE.txt).

The [etags-table/etags-table.el](etags-table/etags-table.el) file is
licensed under [GNU General Public License Version
2](etags-table/LICENSE).
