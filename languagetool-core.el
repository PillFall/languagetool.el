;;; languagetool-core.el --- LanguageTool Core module -*- lexical-binding: t; -*-

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

;; LanguageTool Core functions and packages

;;; Code:

;; Variable definitions:

(require 'ispell)
(eval-when-compile
  (require 'subr-x))

(defcustom languagetool-api-key nil
  "LanguageTool API Key for Premium features."
  :group 'languagetool
  :type '(choice
          (const nil)
          string))

(defcustom languagetool-username nil
  "LanguageTool Username for Premium features.

Your username/email as used to log in at languagetool.org."
  :group 'languagetool
  :type '(choice
          (const nil)
          string))

(defcustom languagetool-correction-language "auto"
  "LanguageTool correction and checking language.

This is a string which indicate the language LanguageTool assume
the text is written in or \"auto\" for automatic calculation."
  :group 'languagetool
  :local t
  :safe #'languagetool-core-safe-language
  :type '(choice
          string
          (const "auto")))

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

(defcustom languagetool-suggestion-level nil
  "If set, additional rules will be activated.

For example, rules that you might only find useful when checking
formal text."
  :group 'languagetool
  :local t
  :type '(choice
          (const nil)
          string))

(defcustom languagetool-core-languages
  '(("auto" . "Automatic Detection")
    ("ar" . "Arabic")
    ("ast-ES" . "Asturian")
    ("be-BY" . "Belarusian")
    ("br-FR" . "Breton")
    ("ca-ES" . "Catalan")
    ("ca-ES-valencia" . "Catalan (Valencian)")
    ("zh-CN" . "Chinese")
    ("da-DK" . "Danish")
    ("nl" . "Dutch")
    ("nl-BE" . "Dutch (Belgium)")
    ("en" . "English")
    ("en-AU" . "English (Australian)")
    ("en-CA" . "English (Canadian)")
    ("en-GB" . "English (GB)")
    ("en-NZ" . "English (New Zealand)")
    ("en-ZA" . "English (South African)")
    ("en-US" . "English (US)")
    ("eo" . "Esperanto")
    ("fr" . "French")
    ("gl-ES" . "Galician")
    ("de" . "German")
    ("de-AT" . "German (Austria)")
    ("de-DE" . "German (Germany)")
    ("de-CH" . "German (Swiss)")
    ("el-GR" . "Greek")
    ("ga-IE" . "Irish")
    ("it" . "Italian")
    ("ja-JP" . "Japanese")
    ("km-KH" . "Khmer")
    ("nb" . "Norwegian (Bokmål)")
    ("no" . "Norwegian (Bokmål)")
    ("fa" . "Persian")
    ("pl-PL" . "Polish")
    ("pt" . "Portuguese")
    ("pt-AO" . "Portuguese (Angola preAO)")
    ("pt-BR" . "Portuguese (Brazil)")
    ("pt-MZ" . "Portuguese (Moçambique preAO)")
    ("pt-PT" . "Portuguese (Portugal)")
    ("ro-RO" . "Romanian")
    ("ru-RU" . "Russian")
    ("de-DE-x-simple-language" . "Simple German")
    ("sk-SK" . "Slovak")
    ("sl-SI" . "Slovenian")
    ("es" . "Spanish")
    ("es-AR" . "Spanish (voseo)")
    ("sv" . "Swedish")
    ("tl-PH" . "Tagalog")
    ("ta-IN" . "Tamil")
    ("uk-UA" . "Ukrainian"))
  "LanguageTool available languages for correction.

Each element is a cons-cell with the form (CODE . NAME)."
  :group 'languagetool
  :type '(alist
          :key-type (string :tag "Code")
          :value-type (string :tag "Name")))

(defvar-local languagetool-correction-language-history nil
  "Buffer local LanguageTool correction language history.")

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
  :safe #'languagetool-core-safe-rules
  :type '(choice
          (const nil)
          (repeat string)))

(defcustom languagetool-hint-function
  'languagetool-core-hint-default-function
  "Display error information in the minibuffer.

The function must search for overlays at point. You must pass the
function symbol.

A example hint function:
\(defun hint-function ()
  \"Hint display function.\"
  (dolist (ov (overlays-at (point)))
    (when (overlay-get ov 'languagetool-message)
      (unless (current-message)
        (message
         \"%s%s\"
         (overlay-get ov 'languagetool-short-message)
         (if (/= 0 (length (overlay-get ov 'languagetool-replacements)))
             (concat
              \" -> (\"
              (string-join (languagetool-core-get-replacements ov) \", \")
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

(defun languagetool-core-safe-language (lang)
  "Return non-nil if LANG is safe to use."
  (assoc lang languagetool-core-languages))

(defun languagetool-core-safe-rules (rules)
  "Return non-nil if RULES is safe to use.
To be valid, RULES must be a list of strings form of \"UPPER_UNDERSCORE\"."
  (and (listp rules)
       (seq-every-p (lambda (rule)
                      (and (stringp rule)
                           (string-match-p "^[A-Z_]+$" rule)))
                    rules)))

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
         "%s%s"
         (overlay-get ov 'languagetool-short-message)
         (if (/= 0 (length (overlay-get ov 'languagetool-replacements)))
             (concat
              " -> ("
              (string-join (languagetool-core-get-replacements ov) ", ")
              ")")
           ""))))))

(defun languagetool-core-get-replacements (overlay)
  "Return the replacements of OVERLAY in a list."
  (let ((replacements (overlay-get overlay 'languagetool-replacements))
        replace)
    (dotimes (index (length replacements))
      (push (alist-get 'value (aref replacements index)) replace))
    (reverse replace)))

(defun languagetool-core-correct-p (word)
  "Return non-nil if WORD is on the LocalWords comment in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (search-forward ispell-words-keyword nil t)
                  (not found))
	(when (re-search-forward (rx
                                  (zero-or-more space)
                                  (group (literal word))
                                  (zero-or-more space)) (line-end-position) t)
          (setq found t)))
      found)))

(provide 'languagetool-core)

;;; languagetool-core.el ends here
