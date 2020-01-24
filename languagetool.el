;;; languagetool.el ---  LanguageTool integration for grammar check

;; Copyright (C) 2020  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package passes the buffer to LanguageTool.

;;; Code:

(require 'json)

(defgroup languagetool nil
  "Customize LanguageTool"
  :prefix "languagetool-"
  :group 'applications)

;; Faces:

(defface languagetool-correction-face
  '((((class mono)) (:inverse-video t :bold t :underline t))
    (((class color)) (:background "red" :foreground "yellow"))
    (t (:bold t)))
  "Face used to visualize where the error is."
  :group 'languagetool)


;; Custom Variables:

(defcustom languagetool-java-bin "java"
  "Java executable path or name."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-java-arguments nil
  "List of string which is passed to java command as arguments.

Described at http://wiki.languagetool.org/command-line-options,
recomends to use:

\(setq langtool-java-user-arguments '(\"-Dfile.encoding=UTF-8\"))"
  :group 'languagetool
  :type '(choice
          (repeat string)
          function))

(defcustom languagetool-language-tool-jar nil
  "Absolute path to LanguageTool command line jar file."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-default-language "auto"
  "Language name which LanguageTool will set for correction.
This is string which indicate locale or `auto'."
  :group 'languagetool
  :type '(choice
          string
          (const auto)))

(defcustom languagetool-mother-tongue nil
  "Your mother tongue language name pass to LanguageTool."
  :group 'languagetool
  :type 'string)

(defcustom languagetool-disabled-rules nil
  "Disabled rules pass to LanguageTool.
String that separated by comma or list of string."
  :group 'languagetool
  :type '(choice
          (list string)
          string))

(defcustom languagetool-error-exists-hook nil
  "Hook run after LanguageTool process found any error(s)."
  :group 'languagetool
  :type 'hook)

(defcustom languagetool-no-error-hook nil
  "Hook run after LanguageTool report no error."
  :group 'languagetool
  :type 'hook)

(defcustom languagetool-finish-hook nil
  "Hook run after cleanup buffer."
  :group 'languagetool
  :type 'hook)


;; Local Variables:

(defvar languagetool-local-disabled-rules nil)
(make-variable-buffer-local 'languagetool-local-disabled-rules)

(defvar languagetool-buffer-process nil)
(make-variable-buffer-local 'languagetool-buffer-process)

(defvar languagetool-mode-line-message nil)
(make-variable-buffer-local 'languagetool-mode-line-message)
(put 'languagetool-mode-line-message 'risky-local-variable t)

(defvar languagetool-mode-line-process nil)
(make-variable-buffer-local 'languagetool-mode-line-process)
(put 'languagetool-mode-line-process 'risky-local-variable t)

(defvar languagetool-output-buffer-name "*LanguageTool Output*")

(defvar languagetool-output-parsed nil)
(make-variable-buffer-local 'languagetool-output-parsed)

(defvar languagetool--debug nil)

(defvar languagetool--correction-keys
  [?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
      ;; suggestions may over 10.
      ;; define rest of alphabet just in case.
      ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
      ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t
      ?u ?v ?w ?x ?y ?z])


;; Functions:

;; Create Functions:

(defun languagetool--parse-java-arguments ()
  "Returns java arguments as a list of strings which will be used
when correcting."
  (let ((command '()))
    (dolist (arg languagetool-java-arguments)
      (when (stringp arg)
        (add-to-ordered-list 'command arg)))
    (add-to-ordered-list 'command "-jar")
    (add-to-ordered-list 'command languagetool-language-tool-jar)
    (add-to-ordered-list 'command "-c")
    (add-to-ordered-list 'command "utf8")
    (add-to-ordered-list 'command "--json")
    (when (stringp languagetool-default-language)
      (if (string= languagetool-default-language "auto")
          (add-to-ordered-list 'command "-adl")
        (progn
          (add-to-ordered-list 'command "-l")
          (add-to-ordered-list 'command languagetool-default-language))))
    (when (stringp languagetool-mother-tongue)
      (add-to-ordered-list 'command "-m")
      (add-to-ordered-list 'command languagetool-mother-tongue))
    (let ((rules ""))
      (dolist (rule languagetool-disabled-rules)
        (when (stringp rule)
          (setq rules (concat rules rule ","))))
      (unless (string= rules "")
        (add-to-ordered-list 'command "-d")
        (add-to-ordered-list 'command rules)))
    (reverse command)))

(defun languagetool--create-overlay (begin end correction)
  "Creates an overlay face for corrections."
  (save-excursion
    (let ((ov (make-overlay begin end))
          (short-message (cdr (assoc 'shortMessage correction)))
          (message (cdr (assoc 'message correction)))
          (replacements (cdr (assoc 'replacements correction)))
          (rule (cdr (assoc 'rule correction))))
      (overlay-put ov 'languagetool-short-message short-message)
      (overlay-put ov 'languagetool-message message)
      (overlay-put ov 'languagetool-replacements replacements)
      (overlay-put ov 'languagetool-rule rule)
      (overlay-put ov 'help-echo short-message)
      (overlay-put ov 'priority 1)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'face 'languagetool-correction-face))))


;; Output and debug functions:

(defun languagetool--write-debug-info (text)
  "Writes to `languagetool-output-buffer-name' the debug
information."
  (let ((current-string " ----- LanguageTool Command:"))
    (put-text-property 0 (length current-string) 'face 'font-lock-warning-face
                       current-string)
    (insert current-string "\n\n"))
  (insert languagetool-java-bin " "
          (mapconcat (lambda (x) (format "%s" x))(languagetool--parse-java-arguments) " ")
          "\n\n\n\n")
  (let ((current-string " ----- LanguageTool Text:"))
    (put-text-property 0 (length current-string) 'face 'font-lock-warning-face
                       current-string)
    (insert current-string "\n\n"))
  (insert text "\n\n\n\n")
  (let ((current-string " ----- LanguageTool Output:"))
    (put-text-property 0 (length current-string) 'face 'font-lock-warning-face
                       current-string)
    (insert current-string "\n\n")))


;; Correction functions:

(defun languagetool--invoke-command-region (begin end)
  "Invoke LanguageTool passing the current region to STDIN."
  (languagetool--clear-buffer)
  (unless (executable-find languagetool-java-bin)
    (error "Java could not be found."))
  (unless languagetool-language-tool-jar
    (error "LanguageTool jar path is not set."))
  (unless (file-readable-p languagetool-language-tool-jar)
    (error "LanguageTool jar is not readable or could not be found."))
  (save-excursion
    (let ((status 0))
      (let ((buffer (get-buffer-create languagetool-output-buffer-name))
            (text (buffer-substring-no-properties begin end)))
        (save-current-buffer
          (set-buffer buffer)
          (erase-buffer)
          (languagetool--write-debug-info text))
      (setq status
            (apply 'call-process-region begin end
                   languagetool-java-bin
                   nil
                   languagetool-output-buffer-name
                   nil
                   (languagetool--parse-java-arguments)))
      (when (/= status 0)
        (error "LanguageTool returned with status %d" status)))
    (let ((buffer (get-buffer languagetool-output-buffer-name))
          (json-parsed nil))
      (save-current-buffer
        (set-buffer buffer)
        (widen)
        (goto-char (point-max))
        (backward-sexp)
        (setq json-parsed (json-read)))
      (setq languagetool-output-parsed json-parsed))))
  (pop-mark))

(defun languagetool--check-corrections-p ()
  "Returns t if corrections can be made or nil otherwise."
  (if (/= 0
         (length (cdr (assoc 'matches languagetool-output-parsed))))
      t
    nil))

(defun languagetool--get-replacements (overlay)
  "Returns the replacements of `overlay' in a list."
  (let ((replacements (overlay-get overlay 'languagetool-replacements))
        (list-replace '()))
    (dotimes (index (length replacements))
      (add-to-ordered-list
       'list-replace
       (cdr (assoc 'value (aref replacements index)))))
    (reverse list-replace)))

(defun languagetool--show-corrections (begin end)
  "Highlight corrections in the buffer."
  (let ((corrections (cdr (assoc 'matches languagetool-output-parsed)))
        (correction nil))
    (dotimes (index (length corrections))
      (setq correction (aref corrections index))
      (let ((offset (cdr (assoc 'offset correction)))
            (size (cdr (assoc 'length correction))))
        (languagetool--create-overlay
         (+ begin offset) (+ begin offset size)
         correction))))
  (setq languagetool-hint--timer
        (run-with-idle-timer languagetool-hint-idle-delay t
                             languagetool-hint-function)))

(defun languagetool--clear-buffer ()
  "Deletes all buffer overlays."
  (save-restriction
    (widen)
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'languagetool-message)
          (delete-overlay ov)))))
  (run-hooks 'languagetool-finish-hook)
  (when languagetool-hint--timer
    (cancel-timer languagetool-hint--timer)))

;;;###autoload
(defun languagetool-check (begin end)
  "Correct region of the current buffer and highlight errors."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (languagetool--invoke-command-region begin end)
  (if (languagetool--check-corrections-p)
      (progn
        (languagetool--show-corrections begin end)
        (run-hooks 'languagetool-error-exists-hook))
    (progn
      (message "LanguageTool finished, found no errors.")
      (run-hooks 'languagetool-no-error-hook))))

;;;###autoload
(defun languagetool-clear-buffer ()
  "Deletes all buffer correction hightlights."
  (interactive)
  (languagetool--clear-buffer)
  (message "Cleaned buffer from LanguageTool."))


;; Hint Message:

(defcustom languagetool-hint-function
  'languagetool-hint-default-function
  "Function which displays information about the error in the
minibuffer. Must search for overlays at point."
  :group 'languagetool
  :type '(choice
          (const nil)
          function))

(defcustom languagetool-hint-idle-delay 0.5
  "Number of seconds while idle time to wait before showing error
message."
  :group 'languagetool
  :type 'number)

(defvar languagetool-hint--timer nil
  "Hold idle timer watch every LanguageTool processed buffer.")

(defun languagetool-hint-default-function ()
  "Default display function to be used by LanguageTool to show
information."
  (dolist (ov (overlays-at (point)))
    (when (overlay-get ov 'languagetool-message)
      (unless (current-message)
        (message
         "%s%s" (overlay-get ov 'languagetool-short-message)
         (if (/= 0
                 (length (overlay-get ov 'languagetool-replacements)))
             (concat
              " -> ("
              (mapconcat
               'identity (languagetool--get-replacements ov) ", ")
              ")")
           ""))))))


;; Correction functions:

(defun languagetool--parse-correction-message (overlay)
  "Parse and style minibuffer correction."
  (let (msg)
    (setq msg (concat
               "[" (cdr (assoc 'id (overlay-get overlay 'languagetool-rule))) "] "))
    (let ((current-string (format "%s" (overlay-get overlay 'languagetool-message))))
        (put-text-property 0 (length current-string)
                           'face 'font-lock-warning-face
                           current-string)
        (setq msg (concat msg current-string "\n")))
    (let ((replacements (languagetool--get-replacements overlay)))
      (when (< 0 (length replacements))
        (let ((num-choices (length replacements)))
          (when (> (length replacements) (length languagetool--correction-keys))
            (setq num-choices (length languagetool--correction-keys))
            (setq msg (concat msg "Not all choices shown.\n")))
          (setq msg (concat msg "\n"))
          (dotimes (index num-choices)
            (let ((current-string (format "%c" (aref languagetool--correction-keys index))))
              (put-text-property 0 (length current-string)
                                 'face 'font-lock-keyword-face
                             current-string)
              (setq msg (concat msg "[" current-string "]: ")))
            (setq msg (concat msg (nth index replacements) "  "))))))
    (let ((current-string "C-i"))
      (put-text-property 0 (length current-string)
                         'face 'font-lock-keyword-face
                         current-string)
      (setq msg (concat msg "\n[" current-string "]: Ignore  ")))
    (let ((current-string "C-s"))
      (put-text-property 0 (length current-string)
                         'face 'font-lock-keyword-face
                         current-string)
      (setq msg (concat msg "[" current-string "]: Skip")))
    msg))

(defun languagetool--do-correction (pressed-key overlay)
  "Correct an deletes the overlay with LanguageTool Suggestion."
  (cond
   ((char-equal ?\C-i pressed-key)
    (progn
      (goto-char (overlay-end overlay))
      (delete-overlay overlay)))
   ((char-equal ?\C-s pressed-key)
    (goto-char (overlay-end overlay)))
   ((not (cl-position pressed-key languagetool--correction-keys))
    (error "Key `%c' cannot be used." pressed-key))
   (t
    (let ((size (length (languagetool--get-replacements overlay)))
          (pos (cl-position pressed-key languagetool--correction-keys)))
      (when (> (1+ pos) size)
        (error "Correction key `%c' cannot be used." pressed-key))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (insert (nth pos (languagetool--get-replacements overlay)))
      (delete-overlay overlay)))))

(defun languagetool--correct-point ()
  "Show correction buffer at point and do correction."
  (catch 'languagetool-correction
    (let (pressed-key)
      (dolist (ov (overlays-at (point)))
        (when (overlay-get ov 'languagetool-message)
          (setq pressed-key
                (read-char (languagetool--parse-correction-message ov)))
          (languagetool--do-correction pressed-key ov))))
    (throw 'languagetool-correction nil)))

;;;###autoload
(defun languagetool-correct-at-point ()
  "Pops up transient buffer which you can select the correction"
  (interactive)
  (languagetool--correct-point))

;;;###autoload
(defun languagetool-correct-buffer ()
  "Pops up transient buffer which you can select the corrections
  to all errors found in current buffer"
  (interactive)
  (save-excursion
    (dolist (ov (reverse (overlays-in (point-min) (point-max))))
      (when (overlay-get ov 'languagetool-message)
        (goto-char (overlay-start ov))
        (languagetool--correct-point)))))


(provide 'languagetool)
;;; languagetool.el ends here
