;;; languagetool-correction.el --- LanguageTool Correction module -*- lexical-binding: t; -*-

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

;; LanguageTool Correction routines and variables.

;;; Code:

(require 'cl-lib)
(require 'languagetool-core)

;; Variable definitions:

(defvar languagetool-correction-keys
  [?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
      ;; suggestions may over 10.
      ;; define rest of alphabet just in case.
      ?a ?b ?c ?d ?e ?f ?g ?h ?i ?j
      ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t
      ?u ?v ?w ?x ?y ?z]
  "LanguageTool suggestion keys.")

;; Function definitions:

(defun languagetool-correction-parse-message (overlay)
  "Parse and style minibuffer correction.

Get the information about corrections from OVERLAY."
  (let ((msg nil)
        (rule (cdr (assoc 'id (overlay-get overlay 'languagetool-rule))))
        (message (overlay-get overlay 'languagetool-message)))
    ;; Add LanguageTool rule to the message
    (setq msg (concat msg "[" rule "] "))

    ;; Add LanguageTool correction suggestion
    (setq msg (concat msg (propertize (format "%s" message) 'face 'font-lock-warning-face) "\n"))

    ;; Format all the possible replacements for the correction suggestion
    (let ((replacements (languagetool-core-get-replacements overlay)))
      (when (< 0 (length replacements))
        (let ((num-choices (length replacements)))
          ;; If can't assoc each replacement with each hotkey
          (when (> (length replacements) (length languagetool-correction-keys))
            (setq num-choices (length languagetool-correction-keys))
            (setq msg (concat msg "Not all choices shown.\n")))
          (setq msg (concat msg "\n"))
          ;; Format all choices
          (dotimes (index num-choices)
            (setq msg (concat msg
                              "["
                              (propertize
                               (format "%c" (aref languagetool-correction-keys index))
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

(defun languagetool-correction-apply (pressed-key overlay)
  "Correct text marked by LanguageTool with user choice.

PRESSED-KEY is the index of the suggestion in the array contained
on OVERLAY."
  (cond
   ((char-equal ?\C-i pressed-key)
    (progn
      (goto-char (overlay-end overlay))
      (delete-overlay overlay)))
   ((char-equal ?\C-s pressed-key)
    (goto-char (overlay-end overlay)))
   ((not (cl-position pressed-key languagetool-correction-keys))
    (error "Key `%c' cannot be used" pressed-key))
   (t
    (let ((size (length (languagetool-core-get-replacements overlay)))
          (pos (cl-position pressed-key languagetool-correction-keys)))
      (when (> (1+ pos) size)
        (error "Correction key `%c' cannot be used" pressed-key))
      (delete-region (overlay-start overlay) (overlay-end overlay))
      (insert (nth pos (languagetool-core-get-replacements overlay)))
      (delete-overlay overlay)))))

(defun languagetool-correction-at-point ()
  "Show issue at point and try to apply suggestion."
  (let (pressed-key)
    (dolist (ov (overlays-at (point)))
      (when (overlay-get ov 'languagetool-message)
        (message nil)
        (setq pressed-key
              (read-char (languagetool-correction-parse-message ov)))
        (languagetool-correction-apply pressed-key ov)))))

(provide 'languagetool-correction)

;;; languagetool-correction.el ends here
