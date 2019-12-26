;;;  -*- lexical-binding: t; -*-

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


;;;###autoload
(define-minor-mode yaemep-etags-auto-gen-mode
  "Automatically create etags TAGS file for Erlang projects"
  :lighter " yaemep-etags")

;;;###autoload

(add-hook 'erlang-mode-hook 'yaemep-etags-auto-gen-mode)

(defun yaemep-etags-auto-gen-mode-toggle ()
  (if yaemep-etags-auto-gen-mode
      (progn
        (yaemep-project-etags-update-in-background)
        (add-hook 'after-save-hook 'yaemep-project-etags-update-in-background nil t))
    (progn
      (remove-hook 'after-save-hook 'yaemep-project-etags-update-in-background t))))

(add-hook 'yaemep-etags-auto-gen-mode-hook
          'yaemep-etags-auto-gen-mode-toggle)

(provide 'yaemep-etags-auto-gen-mode)
