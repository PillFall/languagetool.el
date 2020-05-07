;;; languagetool-issue-faces.el --- LanguegeTool correction faces -*- lexical-binding: t; -*-

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

;; LanguageTool correction faces.  Get from LanguageTool API.
;;
;; Addition, Characters, Duplication, Formatting, Grammar,
;; Inconsistency, InconsistentEntities, Internationalization, Legal,
;; Length, LocaleSpecificContent, LocaleViolation, Markup,
;; Misspelling, Mistranslation, NonConformance, Numbers, Omission,
;; Other, PatternProblem, Register, Style, Terminology, Typographical,
;; Uncategorized, Untranslated, Whitespace

;;; Code:

;; Faces:

(defface languagetool-default-face
  '((((class mono))
     :inverse-video t
     :underline t)
    (((class color))
     :background "yellow"
     :foreground "black")
    (t (:bold t)))
  "Default error face."
  :group 'languagetool)

(defface languagetool-misspelling-face
  '((((class mono))
     :inverse-video t
     :underline t)
    (((class color))
     :background "red"
     :foreground "white")
    (t :bold t))
  "LanguageTool misspelling error face."
  :group 'languagetool)

(defface languagetool-grammar-face
  '((((class mono))
     :inverse-video t
     :underline t)
    (((class color))
     :background "green"
     :foreground "black")
    (t :bold t))
  "LanguageTool grammar error face."
  :group 'languagetool)

(defface languagetool-style-face
  '((((class mono))
     :inverse-video t
     :underline t)
    (((class color))
     :background "blue"
     :foreground "white")
    (t :bold t))
  "LanguageTool style error face."
  :group 'languagetool)


(provide 'languagetool-issue-faces)

;;; languagetool-issue-faces.el ends here
