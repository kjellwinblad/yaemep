;; %CopyrightBegin%
;;
;; Copyright Kjell Winblad (http://winsh.me, kjellwinblad@gmail.com)
;; 2019. All Rights Reserved.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; %CopyrightEnd%

(require 'yaemep)

(defun yaemep-company-complete-or-completion-at-point ()
  "Run company-complete if activated and run completion-at-point
 otherwise"
  (interactive)
  (if (and (boundp 'company-mode) company-mode (fboundp 'company-complete))
      (company-complete)
    (completion-at-point)))


(defun yaemep-completion ()
  (interactive)
  (if (and (boundp 'yaemep-completion-mode)
           yaemep-completion-mode)
      (yaemep-company-complete-or-completion-at-point)
    (with-output-to-temp-buffer "*YAEMEP yaemep-completion-mode not active*"
      (princ
       "

YAEMEP completion does not work when
yaemep-completion-mode is inactive. Please make sure that
yaemep-completion-mode is active (you should have the text
yaemep-comp just above the message and minibuffer area in your
Emacs window). YAMEP install instructions and documentation
should be available here:

https://github.com/kjellwinblad/yaemep"))))

;;;###autoload
(define-minor-mode yaemep-completion-mode
  "Add completion-at-point function for Erlang"
  :lighter " yaemep-comp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-TAB") 'yaemep-completion)
            (define-key map (kbd "C-M-i") 'yaemep-completion)
            map))

;;;###autoload

(add-hook 'erlang-mode-hook 'yaemep-completion-mode)

(defun yaemep-completion-mode-toggle ()
  (if yaemep-completion-mode
      (progn
        (add-hook 'completion-at-point-functions 'yaemep-completion-at-point nil t)
        (add-hook 'after-save-hook 'yaemep-completion-cache-update-in-background nil t)
        (yaemep-completion-cache-update-in-background))
    (progn
      (remove-hook 'completion-at-point-functions 'yaemep-completion-at-point t)
      (remove-hook 'after-save-hook 'yaemep-completion-cache-update-in-background t))))

(add-hook 'yaemep-completion-mode-hook 'yaemep-completion-mode-toggle)

(provide 'yaemep-completion-mode)
