;;; languagetool-core.el --- LanguageTool Core module -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 0.4.3
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

;; LanguageTool Core functions and packages

;;; Code:

;; Variable definitions:

(defcustom languagetool-default-language "auto"
  "LanguageTool correction and checking language.

This is a string which indicate the language LanguageTool assume
the text is written in or \"auto\" for automatic calculation."
  :group 'languagetool
  :local t
  :type '(choice
          string
          (const auto)))

(defcustom languagetool-mother-tongue nil
  "Your mother tongue for being aware of false friends.

As in some languages two differents word can look or sound
similar, but differ in meaning.

For example in English you have the word \"abnegation\", that is
translated to Polish as \"poświęcenie\". But there is a word in
Polish \"abnegacja\" which means slovenliness or untidiness,
which can be mistranslated."
  :group 'languagetool
  :type '(choice
          (const nil)
          string))

(defcustom languagetool-disabled-rules nil
  "LanguageTool global disabled rules."
  :group 'languagetool
  :type '(choice
          (const nil)
          (repeat string)))


(defcustom languagetool-local-disabled-rules nil
  "LanguageTool buffer local disabled rules."
  :group 'languagetool
  :local t
  :type '(choice
          (const nil)
          (repeat string)))

(defcustom languagetool-hint-function
  'languagetool-core-hint-default-function
  "Display error information in the minibuffer.

The function must search for overlays at point. You must pass the
function symbol.

A example hint function:
\(defun `languagetool-core-hint-default-function' ()
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
               #'identity (languagetool-core-get-replacements ov) \", \")
              \")\")
           \"\"))))))"
  :group 'languagetool
  :type '(choice
          (const nil)
          function))

(defcustom languagetool-hint-idle-delay 0.5
  "Number of seconds idle before showing hint."
  :group 'languagetool
  :type 'number)

(defvar languagetool-core-hint-timer nil
  "Idle timer that shows the hint in the minibuffer.")

;; Function defintions:

(defun languagetool-core-clear-buffer ()
  "Deletes all buffer overlays."
  (save-restriction
    (widen)
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'languagetool-message)
          (delete-overlay ov))))))

(defun languagetool-core-hint-default-function ()
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
               #'identity (languagetool-core-get-replacements ov) ", ")
              ")")
           ""))))))

(defun languagetool-core-get-replacements (overlay)
  "Return the replacements of OVERLAY in a list."
  (let ((replacements (overlay-get overlay 'languagetool-replacements))
        (replace nil))
    (dotimes (index (length replacements))
      (setq replace (append replace
                            (list (cdr (assoc 'value (aref replacements index)))))))
    replace))

(provide 'languagetool-core)

;;; languagetool-core.el ends here
