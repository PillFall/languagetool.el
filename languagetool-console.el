;;; languagetool-console.el --- LanguageTool Console commands -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 1.2.0
;; Package-Requires: ((emacs "27.1"))

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

;; LanguageTool commands and variables to use LanguageTool via
;; languagetool-commandline.jar or org.languagetool.commandline.Main.

;;; Code:

(require 'json)
(require 'languagetool-core)
(require 'languagetool-issue)
(require 'languagetool-java)
(eval-when-compile
  (require 'subr-x))

;; Group definition:

(defgroup languagetool-console nil
  "LanguageTool command line parser and checking."
  :tag "Console"
  :prefix "languagetool-console-"
  :group 'languagetool)

;; Variable definitions:

(defcustom languagetool-console-command nil
  "LanguageTool Command Line path or class.

When using LanguageTool you should set this variable to either
the path or the class to call LanguageTool."
  :group 'languagetool-console
  :type '(choice
          file
          string))

(defcustom languagetool-console-arguments nil
  "LanguageTool Command Line extra arguments.

More info at http://wiki.languagetool.org/command-line-options."
  :group 'languagetool-console
  :type '(choice
          (const nil)
          (repeat string)))

(defvar languagetool-console-output-buffer-name "*LanguageTool Output*"
  "LanguageTool Console output buffer for debugging.

This buffer is also used for parsing the output from LanguageTool
Command Line.")

(defvar-local languagetool-console-output-parsed nil
  "LanguageTool Console last parsed output from working on current buffer.")

;;; Function definitions:

(defun languagetool-console-class-p ()
  "Return non-nil if `languagetool-console-command' is a Java class."
  (let ((regex (rx
                line-start
                (zero-or-more
                 (group
                  (in alpha ?_ ?$)
                  (zero-or-more
                   (in alnum ?_ ?$))
                  ?.))
                (in alpha ?_ ?$)
                (zero-or-more
                 (in alnum ?_ ?$))
                line-end)))
    (string-match-p regex languagetool-console-command)))

(defun languagetool-console-command-exists-p ()
  "Return non-nil if `languagetool-console-command' can be used or exists.

Also sets `languagetool-console-command' to a full path if needed
for this package to work."
  (or (languagetool-console-class-p)
      (when (file-readable-p (file-truename languagetool-console-command))
        (setq languagetool-console-command (file-truename languagetool-console-command))
        t)))

(defun languagetool-console-parse-arguments ()
  "Return the LanguageTool Command Line arguments as a list."
  (unless (listp languagetool-console-arguments)
    (error "LanguageTool Console Arguments must be a list of strings"))

  (let (arguments)

    ;; Appends LanguageTool Console Command
    (unless (languagetool-console-class-p)
      (push "-jar" arguments))
    (push languagetool-console-command arguments)

    ;; Appends the LanguageTool arguments
    (push languagetool-console-arguments arguments)

    ;; Appends the common arguments
    (push (list "--encoding" "utf8" "--json") arguments)

    ;; Appends the correction language information
    (if (string= languagetool-correction-language "auto")
        (push "--autoDetect" arguments)
      (push (list "--language" languagetool-correction-language) arguments))

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (push (list "--mothertongue" languagetool-mother-tongue) arguments))

    ;; Appends LanguageTool Suggestion level information
    (when (stringp languagetool-suggestion-level)
      (push (list "--level" (upcase languagetool-suggestion-level)) arguments))

    ;; Appends the disabled rules
    (let ((rules (string-join (append languagetool-disabled-rules languagetool-local-disabled-rules) ",")))
      (unless (string= rules "")
        (push (list "--disable" rules) arguments )))
    (flatten-tree (reverse arguments))))

(defun languagetool-console-write-debug-info (parsed-arguments text)
  "Write debug info in `languagetool-console-output-buffer-name'.

PARSED-ARGUMENTS is a list with all the arguments that are passed
to LanguageTool and Java.

TEXT is the region passed to LanguageTool for checking."
  (insert
   (propertize " ----- LanguageTool Command:" 'face 'font-lock-warning-face)
   "\n\n"
   (string-join
    (append
     (list languagetool-java-bin)
     parsed-arguments)
    " ")
   "\n\n\n\n"
   (propertize " ----- LanguageTool Text:" 'face 'font-lock-warning-face)
   "\n\n"
   text
   "\n\n\n\n"
   (propertize " ----- LanguageTool Output:" 'face 'font-lock-warning-face)
   "\n\n"))

(defun languagetool-console-invoke-command-region (begin end)
  "Invoke LanguageTool passing the current region to STDIN.

The region is delimited by BEGIN and END."
  (languagetool-core-clear-buffer)
  (unless (executable-find languagetool-java-bin)
    (error "Java could not be found"))
  (unless languagetool-console-command
    (error "LanguageTool Console Command is empty"))
  (unless (languagetool-console-command-exists-p)
    (error "LanguageTool Console Command could not be found"))

  (save-excursion
    (let ((status 0)
          (buffer (get-buffer-create languagetool-console-output-buffer-name))
          (text (buffer-substring-no-properties begin end))
          (json-parsed nil)
          (parsed-arguments
           (append
            (languagetool-java-parse-arguments)
            (languagetool-console-parse-arguments))))
      (with-current-buffer buffer
        (erase-buffer)
        (languagetool-console-write-debug-info parsed-arguments text))
      (setq status
            (apply #'call-process-region begin end
                   languagetool-java-bin
                   nil
                   languagetool-console-output-buffer-name
                   nil
                   parsed-arguments))
      (when (/= status 0)
        (error "LanguageTool returned with status %d" status))
      (with-current-buffer buffer
        (widen)
        (goto-char (point-max))
        (backward-sexp)
        (setq json-parsed (json-read)))
      (setq languagetool-console-output-parsed json-parsed)))
  (pop-mark))

(defun languagetool-console-check (begin end)
  "Show LanguageTool Console suggestions in the buffer.

This function checks for the region delimited by BEGIN and END."
  (languagetool-console-invoke-command-region begin end)
  (if (languagetool-console-matches-exists-p)
      (progn
        (message (substitute-command-keys "LangugeTool finished.
Use \\[languagetool-correct-buffer] to correct the buffer."))
        (languagetool-console-highlight-matches begin)
        (run-hooks 'languagetool-error-exists-hook))
    (progn
      (message "LanguageTool finished.
Found no errors.")
      (run-hooks 'languagetool-no-error-hook))))

(defun languagetool-console-matches-exists-p ()
  "Return t if issues where found by LanguageTool or nil otherwise."
  (/= 0 (length (alist-get 'matches languagetool-console-output-parsed))))

(defun languagetool-console-highlight-matches (begin)
  "Highlight issues in the buffer.

BEGIN defines the start of the current region."
  (let ((corrections (alist-get 'matches languagetool-console-output-parsed)))
    (dotimes (index (length corrections))
      (let* ((correction (aref corrections index))
             (offset (alist-get 'offset correction))
             (size (alist-get 'length correction))
             (start (+ begin offset))
             (end (+ begin offset size))
             (word (buffer-substring-no-properties start end)))
        (unless (languagetool-core-correct-p word)
          (languagetool-issue-create-overlay start end correction))))
    (setq languagetool-core-hint-timer
          (run-with-idle-timer languagetool-hint-idle-delay t
                               languagetool-hint-function))))

(provide 'languagetool-console)

;;; languagetool-console.el ends here
