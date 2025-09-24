;;; languagetool-java.el --- LanguageTool Variables related to Java -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022 Joar Buitrago

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

;; All the variables related to Java management in just one place.

;;; Code:

;; Group definition:

(defgroup languagetool-java nil
  "LanguageTool Java related configuration."
  :tag "Java"
  :prefix "languagetool-java-"
  :group 'languagetool)

;; Variable definitions:

(defcustom languagetool-java-bin (executable-find "java")
  "Java executable path."
  :group 'languagetool-java
  :type 'file)

(defcustom languagetool-java-arguments nil
  "Java extra arguments.

Described at http://wiki.languagetool.org/command-line-options,
recommends to use:

\(setq `languagetool-java-arguments' \\='(\"-Dfile.encoding=UTF-8\"))

When using LanguageTool via it classes this variable should be
set to:

\(setq `languagetool-java-arguments'
      \\='(\"-Dfile.encoding=UTF-8\"
        \"-cp\"
        \"/path/to/classes:/path/to/classes\"))

For example to use in Arch Linux (with pacman dependency):

\(setq `languagetool-java-arguments'
      \\='(\"-Dfile.encoding=UTF-8\"
        \"-cp\"
        \"/usr/share/languagetool:/usr/share/java/languagetool/*\"))"
  :group 'languagetool-java
  :type '(choice
          (const nil)
          (repeat string)))

;; Function definitions:

(defun languagetool-java-parse-arguments ()
  "Return Java parsed arguments as a list."
  (unless (listp languagetool-java-arguments)
    (error "LanguageTool Java Arguments must be a list of strings"))

  (let (arguments)

    (push languagetool-java-arguments arguments)

    (flatten-tree (reverse arguments))))

(provide 'languagetool-java)

;;; languagetool-java.el ends here
