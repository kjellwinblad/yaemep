;;;  -*- lexical-binding: t; -*-

(require 'thingatpt)
(require 'erlang)

(defvar yaemep-get-support-escript-path-cache nil)

(defun yaemep-get-support-escript-path ()
  "Returns the path of the Emacs yaemep-mode support escript."
  (or yaemep-get-support-escript-path-cache
      (setq yaemep-get-support-escript-path-cache
            (locate-file "emacs_erlang_yaemep_support.erl" load-path))))

(defvar yaemep-get-support-escript-command-path-cache nil)

(defun yaemep-get-support-escript-command-path ()
  "Returns the path of the escript command."
  (or yaemep-get-support-escript-command-path-cache
      (setq yaemep-get-support-escript-command-path-cache
            (locate-file "escript" exec-path exec-suffixes))))

(defvar yaemep-check-support-escript-cache nil)

(defvar yaemep-check-support-escript-check-message-functions
  "")

(defun yaemep-check-support-escript (functionaliy-name &optional always-print-message)
  "Returns t if the yaemep-mode support escript can be executed.
  Prints a warning message and returns nil if yaemep-mode support
  escript cannot be executed."
  (or yaemep-check-support-escript-cache
      (let* ((escript-in-path (or (yaemep-get-support-escript-command-path) "escript"))
             (escript-ok
              (string-equal
               "OK"
               (with-temp-buffer
                 (progn
                   (call-process
                    escript-in-path nil t nil
                    (yaemep-get-support-escript-path)
                    "check")
                   (buffer-string))))))
        (if (and (not escript-ok)
                 (or always-print-message
                     (not (string-match-p
                           (regexp-quote functionaliy-name)
                           yaemep-check-support-escript-check-message-functions))))
            (progn
              (if (not (string-match-p
                           (regexp-quote functionaliy-name)
                           yaemep-check-support-escript-check-message-functions))
                  (setq yaemep-check-support-escript-check-message-functions
                        (concat yaemep-check-support-escript-check-message-functions
                                functionaliy-name)))
              (message (concat
                        "Cannot execute \"escript %s check\" (%s will not work). "
                        "Please check that the escript program is in your path and "
                        "that it is compatible with the "
                        "yaemep-mode version. "
                        (if always-print-message "" "This message will only be displayed once."))
                       (yaemep-get-support-escript-path)
                       (or functionaliy-name "the currently executing function"))))
        (setq yaemep-check-support-escript-cache escript-ok))))

(defun yaemep-support-escript-exec (async parameter-list &optional functionaliy-name)
  "Executes the yaemep-mode support escript with the elements in
  PARAMETER-LIST as parameters. Returns the a string containing
  the text that the script printed to standard output, if ASYNC
  is nil. The function will return directly with the empty string
  as return value if ASYNC is t."
  (if (not (yaemep-check-support-escript
            (or functionaliy-name
                "the yaemep-support-escript-exec function")))
      ""
    (with-temp-buffer
      (progn
        (eval (append (list 'call-process (yaemep-get-support-escript-command-path) nil
                            (if async 0 t) nil)
                      (list (yaemep-get-support-escript-path))
                      parameter-list))

        (buffer-string)))))

(defun yaemep-project-dir ()
  "Attempts to automatically find the project root directory
based on the path to the file associated with the current
buffer. The yaemep-project-dir function tries the following
strategies in the following order to find the project root
directory:

1. Start at the root of the file system and check if any of the
directories on the path to the file associated with the current
buffer contains a file with one of the following names:
.emacs_erlang_mode_project, rebar.config, mix.exs, erlang.mk. The
first if any of the dictories containing one of the listed files
will be returned.

2. Start at the dictory that contains the file associated with
the current buffer and check if there is any dictory on the path
that contains a dictory with one of the following names: .git,
.svn, .hg, .cvn.  The first if any of the dictories containing
one of the listed directories will be returned.

3. Return the direcory where the file that is associated with the
current buffer is located."
  (interactive)
  (file-name-as-directory
   (yaemep-support-escript-exec nil
                                (list "get_project_dir"
                                      (expand-file-name (buffer-file-name)))
                                "the yaemep-project-dir function")))

(defvar yaemep-etags-auto-gen-extra-dirs nil
  "yaemep-etags-auto-gen-mode will generate a TAGS file
  with tags for the files in the project directory (see
  yaemep-project-dir) and the files that are stored in the
  direcories whose paths are stored in the list that this
  variable holds. Any paths to Erlang/OTP source code directories
  in this list will be ignored if the project directory is an
  Erlang/OTP source code directory.")

(defvar yaemep-etags-auto-gen-search-pattern "**/*.{erl,hrl}"
  "yaemep-etags-auto-gen-mode will use this search pattern
  to find files to generate tags for. See the Erlang
  documentation for the function filelib:wildcard/1 for the
  syntax of search patterns")

(defun yaemep-project-etags-update (&optional
                                           project-root
                                           output-tags-file
                                           search-pattern
                                           extra-directories
                                           async)
  "Runs the etags command with all the files ending with .erl and
.hrl that are located under the PROJECT-ROOT directory. The
resulting tags file will be stored in the file with the path
OUTPUT-TAGS-FILE."
  (interactive
   (let* ((default-project-root (yaemep-project-dir))
          (default-tags-file (concat default-project-root "TAGS"))
          (use-file-dialog nil))
     (list
      (read-directory-name
       (format "Project root (%s):" default-project-root)
       default-project-root
       default-project-root
       t
       nil)
      (read-file-name
       (format "Tags file (%s):" default-tags-file)
       default-project-root
       default-tags-file
       nil
       "TAGS")
      (read-string "Search pattern (see filelib:wildcard/1):"
                   "**/*.{erl,hrl}"
                   nil
                   (list "**/*.{erl,hrl,c,h}"
                         "**/*.{erl,hrl,c,h,java}"
                         "**/*.{erl,hrl,c,h,java,pl,pm}"
                         "**/*.{erl,hrl,c,h,java,pl,pm,C,c++,cc,cpp,cxx,H,h++,hh,hpp,hxx,M,pdb}"
                         "**/*.{erl,hrl}"))
      ;; Don't run asyncroniusly when executed interactively
      nil)))
  (cond
   ((yaemep-check-support-escript "the yaemep-project-etags-update-visit-interactive function" (not async))
    (yaemep-support-escript-exec
     async
     (append
      (list "update_etags_project_dir"
            (expand-file-name (or project-root (yaemep-project-dir)))
            (expand-file-name
             (let ((actual-output-tags-file
                    (or output-tags-file (yaemep-project-dir))))
               (if (file-directory-p actual-output-tags-file)
                   (concat (file-name-as-directory actual-output-tags-file) "TAGS")
                 actual-output-tags-file)))
            (or search-pattern yaemep-etags-auto-gen-search-pattern "**/*.{erl,hrl}"))
      (mapcar 'expand-file-name (or extra-directories yaemep-etags-auto-gen-extra-dirs))))
    (if (called-interactively-p)
        (progn
          (visit-tags-table (yaemep-project-dir))
          (message "Tags for %s updated and saved in %s" project-root output-tags-file))))
   (t
    nil)))

(defun yaemep-project-etags-update-in-background (&optional
                                                         search-pattern
                                                         extra-directories
                                                         project-root
                                                         output-tags-file)
  "Run the etags command in the background on all files with the
.erl and .hrl ending that are inside the project directory
returned by the function yaemep-project-dir. See the
documentation of the Emacs lisp function yaemep-project-dir and
the etags command for more information."
  (interactive)
  (yaemep-project-etags-update
   project-root
   output-tags-file
   search-pattern
   extra-directories
   t))


(defun yaemep-tags-help ()
  "Describe how to activate automatic update of TAGS file."
  (interactive)
  (with-output-to-temp-buffer "*Erlang Tags Help*"
    (princ
     "
Introduction
============

With the Emacs tags system (etags) one can, for example, go to
the definition of a function by putting the cursor/point over the
function name and pressing \"M-.\" (one can go back to the
previous location by pressing \"M-,\"). However, to use the Emacs
tags system, one has to first generate and load a TAGS file with
information about the .erl and .hrl files that are of
interest. One way to generate a TAGS file is to use the menu item
\"Erlang -> TAGS -> Generate and Visit TAGS File\". Pressing this
menu item could get tedious as one has to redo this every time
one wants to include new changes in the TAGS file. The next
section describes how to utilize a function in yaemep-mode to
automatically update the TAGS file in the background every time a
.erl or .hrl file is opened or saved.


Update TAGS File Automatically
------------------------------

Put the following somewhere after (require 'yaemep-start) in your
Emacs init file to automatically update the TAGS file for your
Emacs project every time you open or save a .erl or .hrl file:

(add-hook 'yaemep-mode-hook
          (lambda ()
            (progn
              (yaemep-project-etags-update-in-background)
              (add-hook 'after-save-hook
                        (lambda ()
                          (progn
                            (yaemep-project-etags-update-in-background))) nil t))))

**Important**

The above code requires that you have the escript command in your
path, which you most likely have if you have Erlang installed on
your system. The yaemep-project-etags-update-in-background
function will just print a warning message in the message buffer
and return if it cannot find a working escript program.

The yaemep-project-etags-update-in-background function will
attempt to automatically find the root of the project that the
file that the current buffer is asociated with. The function
should be able to locate most types of Erlang projects that use
one of the build systems: rebar3, mix and erlang.mk. If the
function is unable to find the root of the project based on the
build system, it will fall back to looking for a .git, .svn or
.cvs folder. If the function cannot locate the project root based
on a build system config file or a version control directory, it
will use the folder where the file that is asociated with the
current buffer is stored as the project root. Finally, if the
yaemep-project-etags-update-in-background cannot find the correct
project root for your project, you can force it to use a
particular folder by placing a file called
\".emacs_erlang_mode_project\" in the root of your project. You
can type \"M-x yaemep-project-dir\" to check which directory
yaemep-project-etags-update-in-background will use as the project
directory.



Load Updated TAGS File Automatically
------------------------------------

By default Emacs will ask if you want to revert the tags file
after it has got updated. To avoid this put the following in your
Emacs init file:

(setq tags-revert-without-query 1)


Working with Several Erlang Project in One Emacs Session
--------------------------------------------------------

If you are working with several Erlang projects in the same Emacs
session it is convenient to make Emacs automatically change TAGS
file when you change project. This can be accomplished with an
emacs plugin called
etags-table (https://www.emacswiki.org/emacs/EtagsTable). To
install the etags-table plugin, first download
etags-table.el (http://www.emacswiki.org/emacs/download/etags-table.el
                accessed 2019-12-04) and then put the following in your Emacs
init file (note that the path in the code below needs to be
                changed).

(add-to-list 'load-path
             \"~/src/directory/where/etags-table/is/located\")
(require 'etags-table)
(setq etags-table-search-up-depth 99)")))


(defun yaemep-completion-cache-dir ()
  "Returns the directory in which the Erlang completion cache is stored"
  (expand-file-name
   (concat (file-name-as-directory (locate-user-emacs-file "cache"))
           (file-name-as-directory "erlang_mode_completion_cache"))))

(defun yaemep-completion-cache-update-in-background ()
  "Update the Erlang completion cache for the project in the background"
  (interactive)
  (yaemep-support-escript-exec
   t
   (list "update_completion_cache"
         (yaemep-completion-cache-dir)
         (expand-file-name (buffer-file-name))))
  (setq yaemep-completion-at-point-cache nil))

(defvar-local yaemep-completion-at-point-enabled nil)

(defvar-local yaemep-completion-at-point-cache nil)

(defun yaemep-completion-at-point ()
  ""
  (interactive)
  (let* ((case-fold-search nil)
         (check-complete-regexp
          (lambda (complete-regexp num-of-char-forward)
            (when (and (save-mark-and-excursion
                        (forward-char num-of-char-forward)
                        (re-search-backward complete-regexp nil t 1))
                       (match-string-no-properties 1)
                       (>= (point) (match-beginning 1))
                       (<= (point) (match-end 1)))
              (format "%s" (match-string-no-properties 1)))))
         (complete-module (lambda ()
                            (funcall check-complete-regexp
                                     "^[ \t]+.*[ \t(),<>=!,()=[+-/*]\\([a-z0-9_]*\\):[a-zA-Z0-9_]*" 1)))
         (complete-fun-in-module
          (lambda ()
            (funcall check-complete-regexp
                     "^[ \t]+.*[{ \t(),<>=![+-/*]\\([a-z0-9_]+:[a-zA-Z0-9_]*\\)" 0)))
         (complete-local-fun
          (lambda ()
            (funcall check-complete-regexp
                     "^[ \t]+.*[{ \t(),<>=![+-/*]\\([a-z0-9_]*\\)()" 2)))
         (complete-local-fun-or-module
          (lambda ()
            (funcall check-complete-regexp
                     "^[ \t]+.*[{ \t(),<>=![+-/*]\\([A-Za-z0-9_]*\\)" 0)))
         (complete-var
          (lambda ()
            (funcall check-complete-regexp
                     "^[ \t]+.*[{ \t(),<>=![+-/*]@\\([A-Z]?[A-Za-z0-9_]*\\)" 0)))
         (complete-var-two
          (lambda ()
            (funcall check-complete-regexp
                     "^[ \t]+.*[{ \t(),<>=![+-/*]\\([A-Z][A-Za-z0-9_]*\\)" 0)))
         (last-match-bounds
          (lambda ()
            (list (match-beginning 1) (match-end 1))))
         (get-var-complete-string
          (lambda (end-point)
            (let ((function-start-point
                   (save-excursion
                     (if (not (erlang-beginning-of-clause))
                         end-point
                       (point)))))
              (buffer-substring-no-properties function-start-point end-point))))
         (get-results
          (lambda (bounds command complete-string exit-fun)
            (if (and yaemep-completion-at-point-cache
                     (equal (car yaemep-completion-at-point-cache)
                            (list (buffer-file-name) command complete-string)))
                (append bounds (cdr (cdr (car (cdr yaemep-completion-at-point-cache)))))
              (let* ((file-name (buffer-file-name))
                     (result
                      (list (car bounds)
                            (car (cdr bounds))
                            (split-string
                             (yaemep-support-escript-exec
                              nil
                              (list command
                                    (yaemep-completion-cache-dir)
                                    file-name
                                    complete-string)) ";")
                            :exclusive 'yes
                            :exit-function exit-fun)))
                (setq yaemep-completion-at-point-cache
                      (list (list file-name command complete-string) result))
                result)))))
    (if (not yaemep-completion-at-point-enabled)
        ;; Check if escript is in path
        (if (not (yaemep-check-support-escript
                  "the yaemep-completion-at-point function"))
            (progn
              (message (concat
                        "The yaemep-completion-at-point function "
                        "will be disabled. See the message buffer "
                        "for more information."))
              (setq yaemep-completion-at-point-enabled 'no)
              (yaemep-completion-at-point))
          (progn
            (setq yaemep-completion-at-point-enabled 'yes)
            (if (boundp 'company-require-match)
                ;; Disable company-mode require match in the buffer if
                ;; company-mode is enabled.
                (progn
                  (make-local-variable 'company-require-match)
                  (setq company-require-match nil)))
            (yaemep-completion-cache-update-in-background)
            (add-hook 'after-save-hook
                      (lambda ()
                        (progn
                          (yaemep-completion-cache-update-in-background))) nil t)
            (yaemep-completion-at-point))))
    (cond
     ((equal yaemep-completion-at-point-enabled 'no)
      nil)
     ;; Check if outside function
     ((let* ((clause-start-end
              (save-excursion
                (if (erlang-beginning-of-clause)
                    (let ((start (point)))
                      (erlang-end-of-clause)
                      (list start (point)))
                  (list 1 0))))
             (start (car clause-start-end))
             (end (car (cdr clause-start-end)))
             (current-pos (point)))
        (or (< current-pos start) (> current-pos end)))
      (list (point)
            (point)
            nil
            :exclusive 'yes))
     ;; Check if inside string
     ((thing-at-point-looking-at "\"[^\"\]*\\(\\\\.[^\"\]*\\)*\"" 100)
      (list (point)
            (point)
            nil
            :exclusive 'yes))
     ;; Check if inside comment
     ((thing-at-point-looking-at "%.*$" 100)
      (list (point)
            (point)
            nil
            :exclusive 'yes))
     ((or (funcall complete-var) (funcall complete-var-two))
      (let* ((bounds (funcall last-match-bounds))
             (complete-string
              (funcall get-var-complete-string (car bounds)))
             (exit-fun (lambda (_string _status)
                         (save-excursion
                           (goto-char (- (car bounds) 1))
                           (if (char-equal (char-after) ?@)
                               (delete-char 1))))))
        (funcall get-results bounds
                 "list_local_vars"
                 complete-string exit-fun)))
     ((funcall complete-fun-in-module)
      (let ((complete-string (funcall complete-fun-in-module))
            (bounds (funcall last-match-bounds))
            (exit-fun (lambda (_string _status) nil)))
        (funcall get-results bounds
                 "list_functions_in_module"
                 (concat (car (split-string complete-string ":")) ":")
                 exit-fun)))
     ((funcall complete-module)
      (let ((bounds (funcall last-match-bounds))
            (exit-fun (lambda (_string _status)
                        (delete-char 1)
                        (insert-char ?:))))
        (funcall get-results
                 bounds
                 "list_modules"
                 "not_used"
                 exit-fun)))
     ((funcall complete-local-fun)
      (let ((bounds (funcall last-match-bounds))
            (exit-fun (lambda (_string _status)
                        (delete-char 2))))
        (funcall get-results
                 bounds
                 "list_functions_in_erl_file"
                 "not_used"
                 exit-fun)))
     ((funcall complete-local-fun-or-module)
      (let* ((bounds (funcall last-match-bounds))
             (complete-string
              (funcall get-var-complete-string (car bounds)))
             (exit-fun (lambda (_string _status) nil)))
        (funcall get-results
                 bounds
                 "list_modules_and_functions_in_erl_file"
                 complete-string exit-fun)))
     (t
      (list (point)
            (point)
            nil
            :exclusive 'yes)))))


(provide 'yaemep)
