;;; languagetool-issue.el --- LanguegeTool issue faces -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 1.3.0
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

;; LanguageTool correction faces.  Get from LanguageTool API.
;;
;; Addition, Characters, Duplication, Formatting, Grammar,
;; Inconsistency, InconsistentEntities, Internationalization, Legal,
;; Length, LocaleSpecificContent, LocaleViolation, Markup,
;; Misspelling, Mistranslation, NonConformance, Numbers, Omission,
;; Other, PatternProblem, Register, Style, Terminology, Typographical,
;; Uncategorized, Untranslated, Whitespace

;;; Code:

;; Group definition:

(defgroup languagetool-issue nil
  "LanguageTool faces for marking issues."
  :tag "Issue Faces"
  :prefix "languagetool-issue-"
  :group 'languagetool)

;; Face definitions:

(defface languagetool-issue-default
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "yellow"))
    (t
     :underline t :inherit error))
  "LanguageTool face for default issues."
  :group 'languagetool-issue)

(defface languagetool-issue-misspelling
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "red"))
    (t
     :underline t :inherit error))
  "LanguageTool face for misspelling errors."
  :group 'languagetool-issue)

(defface languagetool-issue-grammar
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "green"))
    (t
     :underline t :inherit error))
  "LanguageTool face for grammar errors."
  :group 'languagetool-issue)

(defface languagetool-issue-style
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "blue"))
    (t
     :underline t :inherit error))
  "LanguageTool face for style errors."
  :group 'languagetool-issue)

(defcustom languagetool-issue-face-alist
  '(("misspelling" . languagetool-issue-misspelling)
    ("grammar" . languagetool-issue-grammar)
    ("style" . languagetool-issue-style))
  "Alist with issue type associated with it's face.

Each element is a cons cell with the form (ISSUE_TYPE . FACE_NAME)."
  :group 'languagetool-issue
  :type '(alist
          :key-type (string :tag "Issue Type")
          :value-type (face :tag "Face Name")))

;; Function definitions:

(defun languagetool-issue-get-face (issue-type)
  "Return the face for ISSUE-TYPE."
  (or (cdr (assoc issue-type languagetool-issue-face-alist))
      'languagetool-issue-default))

(defun languagetool-issue-create-overlay (begin end correction)
  "Create an overlay for corrections.

Create an overlay for correction in the region delimited by BEGIN
and END, parsing CORRECTION as overlay properties."
  (save-excursion
    (let* ((ov (make-overlay begin end))
           (short-message (alist-get 'shortMessage correction))
           (message (alist-get 'message correction))
           (replacements (alist-get 'replacements correction))
           (rule (alist-get 'rule correction))
           (issue-type (alist-get 'issueType rule)))
      (when (string= short-message "")
        (setq short-message message))
      (overlay-put ov 'languagetool-short-message short-message)
      (overlay-put ov 'languagetool-message message)
      (overlay-put ov 'languagetool-replacements replacements)
      (overlay-put ov 'languagetool-rule rule)
      (overlay-put ov 'help-echo short-message)
      (overlay-put ov 'priority 1)
      (overlay-put ov 'evaporate t)
      (overlay-put ov 'face (languagetool-issue-get-face issue-type)))))

(provide 'languagetool-issue)

;;; languagetool-issue.el ends here
