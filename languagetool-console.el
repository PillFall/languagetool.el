;;; languagetool-console.el --- LanguageTool Console commands -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.0") (request "0.3.2"))

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

;; Group definition:

(defgroup languagetool-console nil
  "LanguageTool command line parser and checking"
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
  "Return nil if `languagetool-console-command' is not a Java class."
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
  "Return t is `languagetool-console-command' can be used or exists.

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

  (let ((arguments nil))

    ;; Appends LanguageTool Console Command
    (unless (languagetool-console-class-p)
      (setq arguments (append arguments (list "-jar"))))
    (setq arguments (append arguments (list languagetool-console-command)))

    ;; Appends the LanguageTool arguments
    (setq arguments (append arguments languagetool-console-arguments))

    ;; Appends the common arguments
    (setq arguments (append arguments
                            (list "--encoding" "utf8")
                            (list "--json")))

    ;; Appends the correction language information
    (if (string= languagetool-correction-language "auto")
        (setq arguments (append arguments (list "--autoDetect")))
      (setq arguments (append arguments (list "--language" languagetool-correction-language))))

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (setq arguments (append arguments (list "--mothertongue" languagetool-mother-tongue))))

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
        (setq arguments (append arguments (list "--disable" rules)))))
    arguments))

(defun languagetool-console-write-debug-info (text)
  "Write debug info in `languagetool-console-output-buffer-name'.

The argument TEXT is the region passed to LanguageTool for
checking."
  (insert (propertize " ----- LanguageTool Command:" 'face 'font-lock-warning-face)
          "\n\n")
  (insert languagetool-java-bin " "
          (mapconcat (lambda (x) (format "%s" x)) (append
                                              (languagetool-console-parse-arguments)
                                              (languagetool-java-parse-arguments)) " ")
          "\n\n\n\n")
  (insert (propertize " ----- LanguageTool Text:" 'face 'font-lock-warning-face)
          "\n\n")
  (insert text "\n\n\n\n")
  (insert (propertize " ----- LanguageTool Output:" 'face 'font-lock-warning-face)
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
          (json-parsed nil))
      (with-current-buffer buffer
        (erase-buffer)
        (languagetool-console-write-debug-info text))
      (setq status
            (apply #'call-process-region begin end
                   languagetool-java-bin
                   nil
                   languagetool-console-output-buffer-name
                   nil
                   (append
                    (languagetool-java-parse-arguments)
                    (languagetool-console-parse-arguments))))
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
  (/= 0 (length (cdr (assoc 'matches languagetool-console-output-parsed)))))

(defun languagetool-console-highlight-matches (begin)
  "Highlight issues in the buffer.

BEGIN defines the start of the current region."
  (let ((corrections (cdr (assoc 'matches languagetool-console-output-parsed)))
        (correction nil))
    (dotimes (index (length corrections))
      (setq correction (aref corrections index))
      (let ((offset (cdr (assoc 'offset correction)))
            (size (cdr (assoc 'length correction))))
        (languagetool-issue-create-overlay
         (+ begin offset) (+ begin offset size)
         correction))))
  (setq languagetool-core-hint-timer
        (run-with-idle-timer languagetool-hint-idle-delay t
                             languagetool-hint-function)))

(provide 'languagetool-console)

;;; languagetool-console.el ends here
