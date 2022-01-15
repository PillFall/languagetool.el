;;; languagetool.el --- LanguageTool integration for grammar and spell check -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 0.4.3
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

(require 'json)
;;(require 'request)

;; Group definition:

(defgroup languagetool nil
  "Grammar and spell checking with LanguageTool."
  :tag "LanguageTool"
  :prefix "languagetool-"
  :group 'applications)

(require 'languagetool-correction)
(require 'languagetool-console)
;;(require 'languagetool-server)

;;;###autoload
(defun languagetool-check (begin end)
  "Correct the current buffer and highlight errors.

If region is selected before calling this function, that would be
the region passed as an argument. The region is delimited by
BEGIN and END"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
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

;;;###autoload
(defun languagetool-correct-at-point ()
  "Pops up transient buffer to do correction at point."
  (interactive)
  (languagetool-correction-at-point))

;;;###autoload
(defun languagetool-correct-buffer ()
  "Pops up transient buffer to do corrections at buffer."
  (interactive)
  (save-excursion
    (dolist (ov (reverse (overlays-in (point-min) (point-max))))
      (when (and (overlay-get ov 'languagetool-message)
                 (overlay-start ov))
        (goto-char (overlay-start ov))
        (languagetool-correction-at-point)))))



(provide 'languagetool)

;;; languagetool.el ends here
