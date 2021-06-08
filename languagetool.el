;;; languagetool.el --- LanguageTool integration for grammar and spell check -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 0.3.0
;; Package-Requires: ((emacs "25.1") (request "0.3.2"))

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

;; Use LanguageTool as your grammar, orthography and styling checker
;; tool in Emacs.

;; languagetool is a utility tool to check and show suggestions made
;; by LanguageTool in the buffer.  Also has real time suggestions made
;; by the LanguageTool Server.

;; For using this package you need Java and LanguageTool jar binaries
;; available in your computer.

;;; Code:

(eval-and-compile (require 'languagetool-issue-faces))

(require 'json)
(require 'request)



;; Group:

(defgroup languagetool nil
  "Spell check with LanguageTool."
  :tag "LanguageTool"
  :prefix "languagetool-"
  :group 'applications)



;; Custom Variables:

(defcustom languagetool-java-bin "java"
  "Java executable path."
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



;; Variables related to LanguageTool Command Line:

(defcustom languagetool-language-tool-jar nil
  "Path to LanguageTool Command Line jar file."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-language-tool-arguments nil
  "List of string passed to LanguageTool jar as argument.

More info at http://wiki.languagetool.org/command-line-options"
  :group 'languagetool
  :type '(choice
          (list string)))

(defvar languagetool-output-buffer-name "*LanguageTool Output*")

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



;; Variables related to LanguageTool Server:

(defcustom languagetool-server-language-tool-jar nil
  "Path to LanguageTool server jar file."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-server-url "http://localhost"
  "URL to be used to communicate to LanguageTool Server."
  :group 'languagetool
  :type 'string)

(defcustom languagetool-server-port 8081
  "Port to be used to communicate to LanguageTool Server."
  :group 'languagetool
  :type 'integer)

(defcustom languagetool-server-delayed-commands
  '(after-save-hook)
  "List of the commands that are searched for sending and parsing correction."
  :group 'languagetool
  :type '(repeat (symbol)))

(defvar languagetool-server-output-buffer-name "*LanguageTool Server Output*")

(defvar languagetool-server-process nil
  "A reference to the LanguageTool Server executable.")



;; Shared variables:

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

(defvar languagetool--correction-keys
  [?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
      ;; suggestions may over 10.
      ;; define rest of alphabet just in case.
      ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
      ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t
      ?u ?v ?w ?x ?y ?z])

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
  "Hold idle timer that shows the hint in the minibuffer.")



;; Buffer Local Variables:

(defvar languagetool-local-disabled-rules nil
  "Disabled rules pass to LanguageTool.  Buffer local.
List of strings.")
(make-variable-buffer-local 'languagetool-local-disabled-rules)

(defvar languagetool-output-parsed nil)
(make-variable-buffer-local 'languagetool-output-parsed)

(defvar languagetool-server--started-p nil
  "Tell if the server can be used or not.")
(make-variable-buffer-local 'languagetool-server--started-p)

(defvar languagetool-server--correcting-p nil
  "Tell if we are actually correcting the buffer.")
(make-variable-buffer-local 'languagetool-server--correcting-p)


;; Functions:

;; Start Server Functions:

;;;###autoload
(define-minor-mode languagetool-server-mode
  "Minor mode that highlights LanguageTool corrections."
  :group 'languagetool
  :lighter " LT"
  (languagetool-server--toggle))

;;;###autoload
(defun languagetool-server-start ()
  "Start the LanguageTool Server.

Its not recommended to run this function more than once."
  (interactive)
  (unless (process-live-p languagetool-server-process)
    (unless (executable-find languagetool-java-bin)
      (error "Java could not be found"))
    (unless languagetool-server-language-tool-jar
      (error "LanguageTool Server jar path is not set"))
    (unless (file-readable-p languagetool-server-language-tool-jar)
      (error "LanguageTool Server jar is not readable or could not be found"))

    (let ((buffer (get-buffer-create languagetool-server-output-buffer-name)))
      ;; Clean the buffer before printing the LanguageTool Server Log
      (with-current-buffer buffer
        (erase-buffer))

      ;; Start LanguageTool Server
      (setq languagetool-server-process
            (start-process "*LanguageTool Server*"
                           buffer
                           languagetool-java-bin
                           "-cp"
                           languagetool-server-language-tool-jar
                           "org.languagetool.server.HTTPServer"
                           "--port"
                           (format "%d" languagetool-server-port)))

      ;; Does not block Emacs when close and do not shutdown the server
      (set-process-query-on-exit-flag languagetool-server-process nil))

    ;; Start running the hint iddle timer
    (setq languagetool-hint--timer
          (run-with-idle-timer languagetool-hint-idle-delay t
                               languagetool-hint-function))))

;;;###autoload
(defun languagetool-server-stop ()
  "Stops the LanguageTool Server."
  (interactive)
  (setq languagetool-server--started-p nil)
  (delete-process languagetool-server-process)
  (when languagetool-hint--timer
    (cancel-timer languagetool-hint--timer))

  ;; Delete active server checking timer if still active
  (when languagetool-server--server-check-timer
    (cancel-timer languagetool-server--server-check-timer)))

(defun languagetool-server--toggle ()
  "Enables or disables LanguageTool Server Mode."
  (if languagetool-server-mode
      (progn
        ;; Start checking for LanguageTool server is able to handle
        ;; requests
        (languagetool-server--server-check)

        ;; Start correction in changes
        (add-hook 'after-change-functions #'languagetool-server--check-on-change nil 'local)
        ;; Start correction on hooks
        (dolist (hook (reverse languagetool-server-delayed-commands))
          (add-hook hook #'languagetool-server-check nil 'local)))

    (progn
      ;; Delete the flag of server started.
      (setq languagetool-server--started-p nil)

      ;; Delete all the checking hooks
      (dolist (hook languagetool-server-delayed-commands)
        (remove-hook hook #'languagetool-server-check 'local))
      ;; Delete correction in changes
      (remove-hook 'after-change-functions #'languagetool-server--check-on-change 'local)

      ;; Delete all LanguageTool overlays
      (languagetool-server--clear-buffer))))


;; Server Checking Functions:

(defun languagetool-server--server-check ()
  "Checks if the LanguageTool Server is able to handle requests."
  (unless languagetool-server--started-p
    (request
      (format "%s:%d/v2/languages" languagetool-server-url languagetool-server-port)
      :type "GET"
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (message "LanguageTool Server communication up...")
                  (setq languagetool-server--started-p t)
                  (languagetool-server-check)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (when (or
                       languagetool-server-mode
                       (not languagetool-server--started-p))
                  (languagetool-server--server-check)))))))

(defun languagetool-server--check-on-change (_begin _end _len)
  "Correct the buffer using LanguageTool and show its suggestion.

This function checks for the actual showed region of the buffer
for suggestions.  This function is an alias to
`languagetool-server-check'.

_BEGIN, _END and _LEN are unused as the buffer needs to be check
completely."
  (languagetool-server-check))

(defun languagetool-server-check ()
  "Correct the buffer using LanguageTool and show its suggestion.

This function checks for the actual showed region of the buffer
for suggestions."
  (unless languagetool-server--correcting-p
    (request
      (format "%s:%d/v2/check" languagetool-server-url languagetool-server-port)
      :type "POST"
      :data (languagetool-server--parse-args)
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (languagetool-server--show-corrections (request-response-data response))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (languagetool-server-mode -1)
                (message "[Fatal Error] LanguageTool closed and got error: %S" error-thrown))))))

(defun languagetool-server--parse-args ()
  "Return the server argument list.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
  (let ((arguments (json-new-object)))

    ;; Appends the correction language information
    (setq arguments (json-add-to-object arguments "language" languagetool-default-language))

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (setq arguments (json-add-to-object arguments "motherTongue" languagetool-mother-tongue)))

    ;; Appends the disabled rules
    (let ((rules ""))
      ;; Global disabled rules
      (dolist (rule languagetool-disabled-rules)
        (if (string= rules "")
            (setq rules (concat rules rule))
          (setq rules (concat rules "," rule))))
      ;; Local disabled rules
      (dolist (rule languagetool-local-disabled-rules)
        (if (string= rules "")
            (setq rules (concat rules rule))
          (setq rules (concat rules "," rule))))
      (unless (string= rules "")
        (setq arguments (json-add-to-object arguments "disabledRules" rules))))

    ;; Add the buffer contents
    (setq arguments (json-add-to-object arguments "text"
                                        (buffer-substring-no-properties (point-min) (point-max))))
    arguments))

(defun languagetool-server--show-corrections (json-parsed)
  "Show the suggestions made by LanguageTool in the buffer.

JSON-PARSED is a json object with the suggestions thrown by the
LanguageTool Server."
  (languagetool-server--clear-buffer)
  (let ((corrections (cdr (assoc 'matches json-parsed)))
        (correction nil))
    (dotimes (index (length corrections))
      (setq correction (aref corrections index))
      (let ((offset (cdr (assoc 'offset correction)))
            (size (cdr (assoc 'length correction))))
        (languagetool--create-overlay
         (+ (point-min) offset) (+ (point-min) offset size)
         correction)))))

(defun languagetool-server--clear-buffer ()
  "Deletes all the buffer overlays."
  (save-restriction
    (widen)
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'languagetool-message)
          (delete-overlay ov))))))



;; Create Functions:

(defun languagetool--parse-java-arguments ()
  "Return java arguments list.

Return the arguments as a list of strings which will be used in
the call of LanguageTool when correcting the text."
  (let ((arguments nil))

    ;; Appends arguments given to java
    (dolist (arg languagetool-java-arguments)
      (setq arguments (append arguments (list arg))))

    ;; Appends the LanguageTool jar path
    (setq arguments (append arguments (list "-jar" languagetool-language-tool-jar)))

    ;; Appends the LanguageTool arguments
    (dolist (arg languagetool-language-tool-arguments)
      (setq arguments (append arguments (list arg))))

    ;; Appends the common arguments
    (setq arguments (append arguments (list "-c" "utf8")
                            (list "--json")))

    ;; Appends the correction language information
    (if (string= languagetool-default-language "auto")
        (setq arguments (append arguments (list "-adl")))
      (setq arguments (append arguments (list "-l" languagetool-default-language))))

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (setq arguments (append arguments (list "-m" languagetool-mother-tongue))))

    ;; Appends the disabled rules
    (let ((rules ""))
      ;; Global disabled rules
      (dolist (rule languagetool-disabled-rules)
        (if (string= rules "")
            (setq rules (concat rules rule))
          (setq rules (concat rules "," rule))))
      ;; Local disabled rules
      (dolist (rule languagetool-local-disabled-rules)
        (if (string= rules "")
            (setq rules (concat rules rule))
          (setq rules (concat rules "," rule))))
      (unless (string= rules "")
        (setq arguments (append arguments (list "-d" rules)))))
    arguments))

(defun languagetool--get-face (issue-type)
  "Return the face for the ISSUE-TYPE."
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
  (insert (propertize " ----- LanguageTool Command:" 'face 'font-lock-warning-face)
          "\n\n")
  (insert languagetool-java-bin " "
          (mapconcat (lambda (x) (format "%s" x)) (languagetool--parse-java-arguments) " ")
          "\n\n\n\n")
  (insert (propertize " ----- LanguageTool Text:" 'face 'font-lock-warning-face)
          "\n\n")
  (insert text "\n\n\n\n")
  (insert (propertize " ----- LanguageTool Output:" 'face 'font-lock-warning-face)
          "\n\n"))



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
  (unless languagetool-server-mode
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
        (run-hooks 'languagetool-no-error-hook)))))

;;;###autoload
(defun languagetool-clear-buffer ()
  "Deletes all buffer correction highlight."
  (interactive)
  (unless languagetool-server-mode
    (languagetool--clear-buffer)
    (run-hooks 'languagetool-finish-hook)
    (message "Cleaned buffer from LanguageTool.")))

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
    ;; Add LanguageTool rule to the message
    (setq msg (concat
               "[" (cdr (assoc 'id (overlay-get overlay 'languagetool-rule))) "] "))

    ;; Add LanguageTool correction suggestion
    (setq msg (concat msg
                      (propertize (format "%s" (overlay-get overlay 'languagetool-message))
                                  'face 'font-lock-warning-face) "\n"))

    ;; Format all the possible replacements for the correction suggestion
    (let ((replacements (languagetool--get-replacements overlay)))
      (when (< 0 (length replacements))
        (let ((num-choices (length replacements)))
          ;; If can't assoc each replacement with each hotkey
          (when (> (length replacements) (length languagetool--correction-keys))
            (setq num-choices (length languagetool--correction-keys))
            (setq msg (concat msg "Not all choices shown.\n")))
          (setq msg (concat msg "\n"))
          ;; Format all choices
          (dotimes (index num-choices)
            (setq msg (concat msg "["
                              (propertize (format "%c" (aref languagetool--correction-keys index))
                                          'face 'font-lock-keyword-face)
                              "]: "))
            (setq msg (concat msg (nth index replacements) "  "))))))
    ;; Add default Ignore and Skip options
    (setq msg (concat msg "\n["
                      (propertize "C-i" 'face 'font-lock-keyword-face)
                      "]: Ignore  "))
    (setq msg (concat msg "["
                      (propertize "C-s" 'face 'font-lock-keyword-face)
                      "]: Skip\n"))
    msg))

(defun languagetool--do-correction (pressed-key overlay)
  "Correct an delete the overlay with LanguageTool Suggestion.
The selected correction is given by PRESSED-KEY and the
position, and suggestions are given by OVERLAY."
  (cond
   ((char-equal ?\C-g pressed-key)
    (progn
      (goto-char (overlay-end overlay))
      (setq languagetool-server--correcting-p nil)
      (error "Quit")))
   ((char-equal ?\C-i pressed-key)
    (progn
      (goto-char (overlay-end overlay))
      (delete-overlay overlay)))
   ((char-equal ?\C-s pressed-key)
    (goto-char (overlay-end overlay)))
   ((not (cl-position pressed-key languagetool--correction-keys))
    (progn
      (setq languagetool-server--correcting-p nil)
      (error "Key `%c' cannot be used" pressed-key)))
   (t
    (let ((size (length (languagetool--get-replacements overlay)))
          (pos (cl-position pressed-key languagetool--correction-keys)))
      (when (> (1+ pos) size)
        (setq languagetool-server--correcting-p nil)
        (error "Correction key `%c' cannot be used" pressed-key))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (insert (nth pos (languagetool--get-replacements overlay)))
      (delete-overlay overlay)))))

(defun languagetool--correct-point ()
  "Show correction buffer at point and do correction."
  (setq languagetool-server--correcting-p t)
  (let (pressed-key
        (inhibit-quit t))
    (dolist (ov (overlays-at (point)))
      (when (overlay-get ov 'languagetool-message)
        (message nil)
        (setq pressed-key
              (read-char (languagetool--parse-correction-message ov)))
        (languagetool--do-correction pressed-key ov))))
  (setq languagetool-server--correcting-p nil))

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
