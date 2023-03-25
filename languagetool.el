;;; languagetool.el --- LanguageTool integration for grammar and spell check -*- lexical-binding: t; -*-

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

;; Use LanguageTool as your grammar, orthography and styling checker
;; tool in Emacs.

;; languagetool is a utility tool to check and show suggestions made
;; by LanguageTool in the buffer.  Also has real time suggestions made
;; by the LanguageTool Server.

;; For using this package you need Java and LanguageTool jar binaries
;; available in your computer.

;;; Code:

(require 'languagetool-correction)
(require 'languagetool-console)
(require 'languagetool-server)

;; Group definition:

(defgroup languagetool nil
  "Grammar and spell checking with LanguageTool."
  :tag "LanguageTool"
  :prefix "languagetool-"
  :group 'applications)

;; Function definitions:

;;;###autoload
(defun languagetool-set-language (lang)
  "Set LanguageTool correction language to LANG."
  (interactive
   (list (read-string "LanguageTool new language: "
                      (cdr (assoc languagetool-correction-language languagetool-core-languages))
                      languagetool-correction-language-history
                      (let (languages-choices)
                        (dolist (language
                                 languagetool-core-languages
                                 (reverse languages-choices))
                          (push (cdr language) languages-choices))))))
  (setq languagetool-correction-language (or (car (rassoc lang languagetool-core-languages))
                                             lang)))

;;;###autoload
(defun languagetool-clear-suggestions ()
  "Clear all the buffer suggestions.

If `languagetool-server-mode' is active, it would rise an error,
as you are not suppose to call this function."
  (interactive)
  (when languagetool-server-mode
    (error "Do not use this function in server mode
If you want to clear the suggestions turn off the server mode"))
  (languagetool-core-clear-buffer))

;;;###autoload
(defun languagetool-check (begin end)
  "Correct the current buffer and highlight errors.

If region is selected before calling this function, that would be
the region passed as an argument. The region is delimited by
BEGIN and END.

If `languagetool-server-mode' is active, send a request to the
server and ends. The parameters BEGIN and END did not make any
difference, as in this mode, the whole buffer needs to be
checked."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (if languagetool-server-mode
      (languagetool-server-send-request)
    (languagetool-console-check begin end)))

;;;###autoload
(defun languagetool-correct-at-point ()
  "Pops up transient buffer to do correction at point."
  (interactive)
  (when languagetool-server-mode
    (setq languagetool-server-correcting-p t))
  (languagetool-correction-at-point)
  (when languagetool-server-mode
    (setq languagetool-server-correcting-p nil)))

;;;###autoload
(defun languagetool-correct-buffer ()
  "Pops up transient buffer to do correction in the whole buffer."
  (interactive)
  (when languagetool-server-mode
    (setq languagetool-server-correcting-p t))
  (condition-case err
      (save-excursion
        (dolist (ov (reverse (overlays-in (point-min) (point-max))))
          (when (and (overlay-get ov 'languagetool-message)
                     (overlay-start ov))
            (goto-char (overlay-start ov))
            (languagetool-correction-at-point))))
    ((quit error)
     (when languagetool-server-mode
       (setq languagetool-server-correcting-p nil))
     (error "%s" (error-message-string err))))
  (when languagetool-server-mode
    (setq languagetool-server-correcting-p nil)))

(provide 'languagetool)

;;; languagetool.el ends here
