;;; languagetool.el --- LanguageTool integration for grammar check -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 0.2.0
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Correct the buffer or region with LanguageTool and show its
;; suggestions in the buffer.

;;; Code:

(eval-and-compile (require 'languagetool-issue-faces))

(require 'json)
(require 'url)

(defgroup languagetool nil
  "LanguageTool's customization group."
  :prefix "languagetool-"
  :group 'applications)

;; Custom Variables:

(defcustom languagetool-java-bin "java"
  "Java executable path or name."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-java-arguments nil
  "List of string passed to java command as arguments.

Described at http://wiki.languagetool.org/command-line-options,
recommends to use:

\(setq `langtool-java-arguments' '(\"-Dfile.encoding=UTF-8\"))"
  :group 'languagetool
  :type '(choice
          (list string)))

(defcustom languagetool-language-tool-jar nil
  "Absolute path to LanguageTool command line jar file."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-language-tool-arguments nil
  "List of string passed to LanguageTool jar as argument.

More info at http://wiki.languagetool.org/command-line-options"
  :group 'languagetool
  :type '(choice
          (list string)))

(defcustom languagetool-default-language "auto"
  "Language name which LanguageTool will set for correction.
This is string which indicate locale or \"auto\"."
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
List of strings."
  :group 'languagetool
  :type '(choice
          (list string)))

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

(defvar languagetool--correction-keys
  [?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
      ;; suggestions may over 10.
      ;; define rest of alphabet just in case.
      ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
      ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t
      ?u ?v ?w ?x ?y ?z])

(defvar languagetool-output-buffer-name "*LanguageTool Output*")

(defcustom languagetool-hint-function
  'languagetool-hint-default-function
  "Display error information in the minibuffer.

The function must search for overlays at point.
You must pass the function symbol.

A example hint function:
\(defun `languagetool-hint-default-function' ()
  \"Default hint display function.\"
  (dolist (ov (overlays-at (point)))
    (when (overlay-get ov 'languagetool-message)
      (unless (current-message)
        (message
         \"%s%s\" (overlay-get ov 'languagetool-short-message)
         (if (/= 0
                 (length (overlay-get ov 'languagetool-replacements)))
             (concat
              \" -> (\"
              (mapconcat
               #'identity (languagetool--get-replacements ov) \", \")
              \")\")
           \"\"))))))"
  :group 'languagetool
  :type '(choice
          (const nil)
          function))

(defcustom languagetool-hint-idle-delay 0.5
  "Number of seconds while idle to wait before showing hint."
  :group 'languagetool
  :type 'number)

(defvar languagetool-hint--timer nil
  "Hold idle timer watch every LanguageTool processed buffer.")

;; Local Variables:

(defvar languagetool-local-disabled-rules nil
  "Disabled rules pass to LanguageTool.  Buffer local.
List of strings.")
(make-variable-buffer-local 'languagetool-local-disabled-rules)

(defvar languagetool-output-parsed nil)
(make-variable-buffer-local 'languagetool-output-parsed)


;; Functions:

;; Create Functions:

(defun languagetool--parse-java-arguments ()
  "Return java arguments list.

Return java arguments as a list of strings which will be used
when correcting."
  (let ((arguments nil))
    (dolist (arg languagetool-java-arguments)
      (setq arguments (append arguments (list arg))))
    (setq arguments (append arguments (list "-jar" languagetool-language-tool-jar)))
    (dolist (arg languagetool-language-tool-arguments)
      (setq arguments (append arguments (list arg))))
    (setq arguments (append arguments (list "-c" "utf8")
                            (list "--json")))
    (if (string= languagetool-default-language "auto")
        (setq arguments (append arguments (list "-adl")))
      (setq arguments (append arguments (list "-l" languagetool-default-language))))
    (when (stringp languagetool-mother-tongue)
      (setq arguments (append arguments (list "-m" languagetool-mother-tongue))))
    (let ((rules ""))
      (dolist (rule languagetool-disabled-rules)
        (if (string= rules "")
            (setq rules (concat rules rule))
          (setq rules (concat rules "," rule))))
      (dolist (rule languagetool-local-disabled-rules)
        (if (string= rules "")
            (setq rules (concat rules rule))
          (setq rules (concat rules "," rule))))
      (unless (string= rules "")
        (setq arguments (append arguments (list "-d" rules)))))
    arguments))

(defun languagetool--get-face (issue-type)
  "Get the face for the ISSUE-TYPE."
  (cond
   ((string= issue-type "misspelling")
    'languagetool-misspelling-face)
   ((string= issue-type "grammar")
    'languagetool-grammar-face)
   ((string= issue-type "style")
    'languagetool-style-face)
   (t
    'languagetool-default-face)))

(defun languagetool--create-overlay (begin end correction)
  "Create an overlay for corrections.

Create an overlay for correction in the region delimited by BEGIN
and END, parsing CORRECTION as overlay properties."
  (save-excursion
    (let* ((ov (make-overlay begin end))
           (short-message (cdr (assoc 'shortMessage correction)))
           (message (cdr (assoc 'message correction)))
           (replacements (cdr (assoc 'replacements correction)))
           (rule (cdr (assoc 'rule correction)))
           (issue-type (cdr (assoc 'issueType rule))))
      (when (string= short-message "")
        (setq short-message message))
      (overlay-put ov 'languagetool-short-message short-message)
      (overlay-put ov 'languagetool-message message)
      (overlay-put ov 'languagetool-replacements replacements)
      (overlay-put ov 'languagetool-rule rule)
      (overlay-put ov 'help-echo short-message)
      (overlay-put ov 'priority 1)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'face (languagetool--get-face issue-type)))))


;; Output and debug functions:

(defun languagetool--write-debug-info (text)
  "Write debug info in `languagetool-output-buffer-name'.

The argument TEXT is the region passed to LanguageTool for
checking."
  (let ((current-string " ----- LanguageTool Command:"))
    (put-text-property 0 (length current-string) 'face 'font-lock-warning-face
                       current-string)
    (insert current-string "\n\n"))
  (insert languagetool-java-bin " "
          (mapconcat (lambda (x) (format "%s" x)) (languagetool--parse-java-arguments) " ")
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
  "Invoke LanguageTool passing the current region to STDIN.

The region is delimited by BEGIN and END."
  (languagetool--clear-buffer)
  (unless (executable-find languagetool-java-bin)
    (error "Java could not be found"))
  (unless languagetool-language-tool-jar
    (error "LanguageTool jar path is not set"))
  (unless (file-readable-p languagetool-language-tool-jar)
    (error "LanguageTool jar is not readable or could not be found"))
  (save-excursion
    (let ((status 0)
          (buffer (get-buffer-create languagetool-output-buffer-name))
          (text (buffer-substring-no-properties begin end))
          (json-parsed nil))
      (with-current-buffer buffer
        (erase-buffer)
        (languagetool--write-debug-info text))
      (setq status
            (apply #'call-process-region begin end
                   languagetool-java-bin
                   nil
                   languagetool-output-buffer-name
                   nil
                   (languagetool--parse-java-arguments)))
      (when (/= status 0)
        (error "LanguageTool returned with status %d" status))
      (with-current-buffer buffer
        (widen)
        (goto-char (point-max))
        (backward-sexp)
        (setq json-parsed (json-read)))
      (setq languagetool-output-parsed json-parsed)))
  (pop-mark))

(defun languagetool--check-corrections-p ()
  "Return t if corrections can be made or nil otherwise."
  (if (/= 0
          (length (cdr (assoc 'matches languagetool-output-parsed))))
      t
    nil))

(defun languagetool--get-replacements (overlay)
  "Return the replacements of OVERLAY in a list."
  (let ((replacements (overlay-get overlay 'languagetool-replacements))
        (replace '()))
    (dotimes (index (length replacements))
      (setq replace (append replace
                            (list (cdr (assoc 'value (aref replacements index)))))))
    replace))

(defun languagetool--show-corrections (begin)
  "Highlight corrections in the buffer.

BEGIN defines the start of the current region."
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
  (when languagetool-hint--timer
    (cancel-timer languagetool-hint--timer)))

;;;###autoload
(defun languagetool-check (begin end)
  "Correct the current buffer and highlight errors.

If region is selected before calling this function it would be
passed as an argument.
The region is delimited by BEGIN and END"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (languagetool--invoke-command-region begin end)
  (if (languagetool--check-corrections-p)
      (progn
        (message (substitute-command-keys "LangugeTool finished.
Use \\[languagetool-correct-buffer] to correct the buffer."))
        (languagetool--show-corrections begin)
        (run-hooks 'languagetool-error-exists-hook))
    (progn
      (message "LanguageTool finished.
Found no errors.")
      (languagetool--clear-buffer)
      (run-hooks 'languagetool-no-error-hook))))

;;;###autoload
(defun languagetool-clear-buffer ()
  "Deletes all buffer correction highlight."
  (interactive)
  (languagetool--clear-buffer)
  (run-hooks 'languagetool-finish-hook)
  (message "Cleaned buffer from LanguageTool."))

;;;###autoload
(defun languagetool-set-language (lang)
  "Change LanguageTool correction language to LANG."
  (interactive
   (list (read-string "Language: " nil nil 'auto)))
  (setq languagetool-default-language lang))


;; Hint Message:

(defun languagetool-hint-default-function ()
  "Default hint display function."
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
               #'identity (languagetool--get-replacements ov) ", ")
              ")")
           ""))))))


;; Correction functions:

(defun languagetool--parse-correction-message (overlay)
  "Parse and style minibuffer correction.

Get the information about corrections from OVERLAY."
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
      (setq msg (concat msg "[" current-string "]: Skip\n")))
    msg))

(defun languagetool--do-correction (pressed-key overlay)
  "Correct an delete the overlay with LanguageTool Suggestion.
The selected correction is given by PRESSED-KEY and the
position, and suggestions are given by OVERLAY."
  (cond
   ((char-equal ?\C-i pressed-key)
    (progn
      (goto-char (overlay-end overlay))
      (delete-overlay overlay)))
   ((char-equal ?\C-s pressed-key)
    (goto-char (overlay-end overlay)))
   ((not (cl-position pressed-key languagetool--correction-keys))
    (error "Key `%c' cannot be used" pressed-key))
   (t
    (let ((size (length (languagetool--get-replacements overlay)))
          (pos (cl-position pressed-key languagetool--correction-keys)))
      (when (> (1+ pos) size)
        (error "Correction key `%c' cannot be used" pressed-key))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (insert (nth pos (languagetool--get-replacements overlay)))
      (delete-overlay overlay)))))

(defun languagetool--correct-point ()
  "Show correction buffer at point and do correction."
  (catch 'languagetool-correction
    (let (pressed-key)
      (dolist (ov (overlays-at (point)))
        (when (overlay-get ov 'languagetool-message)
          (message nil)
          (setq pressed-key
                (read-char (languagetool--parse-correction-message ov)))
          (languagetool--do-correction pressed-key ov)
          (throw 'languagetool-correction nil))))))

;;;###autoload
(defun languagetool-correct-at-point ()
  "Pops up transient buffer to do correction at point."
  (interactive)
  (languagetool--correct-point))

;;;###autoload
(defun languagetool-correct-buffer ()
  "Pops up transient buffer to do corrections at buffer."
  (interactive)
  (save-excursion
    (dolist (ov (reverse (overlays-in (point-min) (point-max))))
      (when (and (overlay-get ov 'languagetool-message)
                 (overlay-start ov))
        (goto-char (overlay-start ov))
        (languagetool--correct-point)))))


(provide 'languagetool)

;;; languagetool.el ends here
