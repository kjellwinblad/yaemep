
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

1. Make
2. Download YAEMEP (e.g., by running `git clone https://github.com/kjellwinblad/yaemep.git`)
3. Put the following in your Emacs init file:
   
           (add-to-list 'load-path "/path/to/yaemep")
           (erlang-yaemep-install-all-extensions-with-default-settings)
4. (Optional) Install and activate company-mode to get modern
   in-buffer autocompletion:
   1. Install company-mode using alternative 1 or 2:
      * Alternative 1 (Ubuntu and Debian based systems) -- Run the
        command:
        
                sudo apt-get install elpa-company
      * Alternative 2 (Should work on all systems) -- Put the following
        lines in your Emacs init file:
        
                (progn
                  (require 'package)
                  (package-initialize)
                  (or (file-exists-p package-user-dir)
                      (package-refresh-contents))
                  (unless (package-installed-p 'company)
                    (package-install 'company)))
   2. Make sure that company-mode is activated when erlang-mode is
      loaded by putting the following in your Emacs init file:
      
              (add-hook 'erlang-mode-hook
                (lambda ()
                  (require 'company)
                  (local-set-key (kbd "M-SPC") 'company-complete)
                  ;; (company-idle-delay nil) ;; Uncomment to disable automatic completion
                  (company-mode)))
3. Restart Emacs

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
