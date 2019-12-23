
YAEMEP -- Yet Another Emacs Erlang Mode Extensions Package
==========================================================

YAEMEP is an Emacs package that contains extensions to the major mode
for Emacs called
[erlang-mode](https://erlang.org/doc/apps/tools/erlang_mode_chapter.html). YAEMEP
does not require the user to do any project-specific setup, and its
only dependencies are that erlang-mode and the escript program are
installed (escript is included in standard Erlang/OTP
installations). One can easily select to use only a subset of the
extensions provided by YAEMEP. YAEMEP contains the following
extensions ([Emacs minor
modes](https://www.gnu.org/software/emacs/manual/html_node/emacs/Minor-Modes.html)):

* **yaemep-completion-mode** provides completion of module names,
  function names in a module, module-local functions, and local
  variables. A function is added to Emacs built-in
  [completion-at-point-functions
  hook](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html)
  when yaemep-completion-mode is activated.
* **yaemep-etags-auto-gen-mode** provides automatic generation of
  etags for Erlang projects (this makes it possible to, e.g., go to
  the function under the point by pressing "`M-.`")
* **yaemep-extra-erlang-menu-mode** adds a menu with shortcuts to
  useful YAEMEP and erlang-mode Emacs functions.

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
   ;; (The line below needs to be changed!) Add YAEMEP to your Emacs load-path:
   (add-to-list 'load-path "/the/path/to/your/yaemep/")

   ;; Install yaemep-completion-mode
   ;; (Completion command can be invoked with "C-M-i")
   (require 'yaemep-completion-mode)

   ;; Install yaemep-etags-auto-gen-mode
   ;; (Use "M-." to go to thing at point and "M-," to go back")
   (require 'yaemep-etags-auto-gen-mode)

   ;; Install yaemep-extra-erlang-menu-mode
   (require 'yaemep-extra-erlang-menu-mode)
   ```
5. (**Optional**) Install and activate company-mode to get modern
   in-buffer autocompletion:
   1. Install [company-mode](https://company-mode.github.io/), for
      example, using one of the two options below:
      * **Option 1** (Ubuntu and Debian based systems) -- Run the
        command:
        
                sudo apt-get install elpa-company
      * **Option 2** (Should work on all systems) -- Put the following
        lines in your Emacs init file:

        ```elisp
        ;; Uncomment the line below to get rid of "Failed to verify signature error"
        ;; (setq package-check-signature nil)
        ;; See https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure

        (require 'package)
        (package-initialize)
        (unless (package-installed-p 'company)
          (progn
            (or (file-exists-p package-user-dir)
                (package-refresh-contents))
            (package-install 'company)))
        ```
   2. Make sure that company-mode is activated when erlang-mode is
      loaded by putting the following in your Emacs init file:

      ```elisp
      (add-hook 'erlang-mode-hook 'company-mode)
      ```
6. (**Optional**) Configure yaemep-etags-auto-gen-mode by putting the
   code below somewhere after `(require 'yaemep-etags-auto-gen-mode)`
   in your Emacs init file and change the variables to fit your setup:

   ```elisp
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
   ```
7. (**Optional**) Load etags TAGS file automatically after it has gotten
   updated:
   ```elisp
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
   ```
8. Restart Emacs


Test YAEMEP in Emacs Without Installing
---------------------------------------

Run the following commands in a command prompt to bring up an Emacs
instance with YAEMEP installed (this will not change your normal Emacs
configuration in any way):

```bash
git clone https://github.com/kjellwinblad/yaemep.git
./yaemep/example/emacs_with_erlang_and_yaemep.sh
```


User Guide
----------

### Completion with `yaemep-completion-mode`

Press the shortcut for completion somewhere inside an Erlang function
definition to trigger completion. The shortcut for completion is
"`C-M-i`" (i.e., the `CTRL`-key, `ALT`-key, and the `i`-key at the
same time on most systems). The shortcut for completion can easily be
changed by putting something like the following in your Emacs init
file:

``` elisp
;; Invoke completion by pressing the ALT-key and space-key at the same time:
(define-key yaemep-completion-mode-map (kbd "M-SPC") 'yaemep-completion)
```


The line that the cursor/point is on needs to be inside an Erlang
function and start with a space or a tab character for completion to
work. Below is a list of the types of completions that are available:

* **modules, local functions, and variables** -- Press the completion
  shortcut when there is a space before the cursor/point
* **Only modules** -- Press the completion when the cursor is just before
  a colon character (i.e., `:`).
* **Only local functions** -- Press the completion key when the
  cursor/point is just before the string `()`.
* **Only variables** -- Press the completion key when the cursor/point
  is directly after an `@`-character

### Go to Function at Point etc with `yaemep-etags-auto-gen-mode`

The Emacs minor-mode `yaemep-etags-auto-gen-mode` automatically
generates a `TAGS` file for Erlang projects that allows you to go the
project function under point, among other things. Only `.erl` and
`.hrl` files in the current project will be included in the `TAGS`
file by default, but this can be changed by setting the variables
`yaemep-etags-auto-gen-extra-dirs` and
`yaemep-etags-auto-gen-search-pattern`. See the install instructions
in the Install section above for details about how to set these
variables. Here is a list of useful shortcuts:

* **`C-.`** Go to function under point
* **`C-,`** Go back to the original place after you have executed "go to
  function under point"

### Menu with `yaemep-extra-erlang-menu-mode`

The Emacs minor-mode `yaemep-extra-erlang-menu-mode` will add a menu to
the top menu bar in Emacs with the title "Erlang YAEMEP". This menu
contains useful shortcuts and key-combinations that can be used to
trigger them.

### How YAEMEP Finds the Project Root

YAEMEP's functions attempt to find the root directory of the Erlang
project automatically. YAEMEP should be able to locate the root
directory of most types of Erlang projects. To find the project root
YAEMEP first tries to find a configuration file for one of the build
systems `rebar3`, `mix`, and `erlang.mk`. If the root directory of the
project can't be found based on the build system, YAEMEP will look for
a `.git`, `.svn` or `.cvs` folder (version control system folders). If
YAEMEP cannot locate the project root using the methods described
above, the directory where the file that is associated with the
current buffer is stored will be used as the project root. Finally,
you can force YAEMEP to use a particular folder by placing a file
called "`.emacs_erlang_mode_project`" in the root directory of your
project. You can type "`M-x yaemep-project-dir`" to check which
directory YAEMEP will use as the project directory.

Links to Similar Projects
-------------------------

Here is a list of projects that provides functionality that is similar
to the functionality that YAEMEP provides:

* [erlang_ls](https://github.com/erlang-ls/erlang_ls) is a [language
  server](https://github.com/microsoft/language-server-protocol)
  implementation for Erlang that can work with Emacs and many other
  editors. erlang_ls is a much more ambitious project than YAEMEP.
* [ivy-erlang-complete](https://github.com/s-kostyaev/ivy-erlang-complete)
  provides slightly more functionality than YAEMEP (e.g., completions
  for macros and records). The ivy-erlang-complete package depends on
  several Emacs third-party packages and system tools (e.g., ivy and
  ag) while YAEMEP's only dependencies are erlang-mode and escript.
* [distel-completion](https://github.com/sebastiw/distel-completion)
  provides completion backends for
  [Distel](https://github.com/massemanet/distel).


License
-------

YAEMEP is licensed under [Apache License Version 2.0](LICENSE.txt).

The [etags-table/etags-table.el](etags-table/etags-table.el) file is
licensed under [GNU General Public License Version
2](etags-table/LICENSE).
