;;; languagetool-server.el --- LanguageTool server mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 0.2.0
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

;; Correct the buffer in real time with LanguageTool and show its
;; suggestions in the buffer.

;;; Code:

(eval-and-compile (require 'languagetool-issue-faces))
(eval-and-compile (require 'languagetool))

(require 'request)



;; Custom Variables:

(defcustom languagetool-server-language-tool-jar nil
  "Absolute path to LanguageTool server jar file."
  :group 'languagetool
  :type 'file)

(defcustom languagetool-server-url "http://localhost"
  "URL to be used to communicate to LanguageTool Server."
  :group 'languagetool
  :type 'string)

(defcustom languagetool-server-port 8081
  "Port to be used to communicate to LanguageTool Server."
  :group 'languagetool
  :type 'integer)

(defcustom languagetool-server-delayed-commands
  '(after-save-hook)
  "List of the commands that are searched for sending and parsing correction."
  :group 'languagetool
  :type '(repeat (symbol)))

(defvar languagetool-server-output-buffer-name "*LanguageTool Server Output*")

(defvar languagetool-server-process nil
  "A reference to the LanguageTool Server executable.")

(defvar languagetool-server--start-check-timer nil
  "Hold idle timer watch every LanguageTool processed buffer.")

(defvar languagetool-server--start-check-delay 0.2
  "Number of seconds while idle to wait before checking again for initialized server.")

(defvar languagetool-server--started-p nil
  "Tell if the server can be used or not.")

(defvar languagetool-server--correcting-p nil
  "Tell if we are actually correcting the buffer")



;; Functions:

;; Start Server Functions:

;;;###autoload
(define-minor-mode languagetool-server-mode
  "Minor mode that highlights LanguageTool corrections."
  :group 'languagetool
  :lighter " LT"
  (languagetool-server--toggle))

;;;###autoload
(defun languagetool-server-start ()
  "Start the LanguageTool Server executable.

Its not recommended to run this function more than once."
  (interactive)
  (unless (process-live-p languagetool-server-process)
    (unless (executable-find languagetool-java-bin)
      (error "Java could not be found"))
    (unless languagetool-server-language-tool-jar
      (error "LanguageTool Server jar path is not set"))
    (unless (file-readable-p languagetool-server-language-tool-jar)
      (error "LanguageTool Server jar is not readable or could not be found"))

    (let ((buffer (get-buffer-create languagetool-server-output-buffer-name)))
            (with-current-buffer buffer
              (erase-buffer))
           (setq languagetool-server-process
                  (start-process "*LanguageTool Server*"
                                 buffer
                                 languagetool-java-bin
                                 "-cp"
                                 languagetool-server-language-tool-jar
                                 "org.languagetool.server.HTTPServer"
                                 "--port"
                                 (format "%d" languagetool-server-port))))

    (setq languagetool-hint--timer
          (run-with-idle-timer languagetool-hint-idle-delay t
                               languagetool-hint-function))))

;;;###autoload
(defun languagetool-server-stop ()
  "Stops the LanguageTool Server executable."
  (interactive)
  (setq languagetool-server--started-p nil)
  (delete-process languagetool-server-process)
  (when languagetool-hint--timer
    (cancel-timer languagetool-hint--timer))

  ;; Delete active server checking timer if still active
  (when languagetool-server--start-check-timer
    (cancel-timer languagetool-server--start-check-timer)))

(defun languagetool-server--toggle ()
  "Start or closes LanguageTool Server."
  (if languagetool-server-mode
      (progn
        ;; Start checking for LanguageTool server open
        (setq languagetool-server--start-check-timer
              (run-with-timer 0 languagetool-server--start-check-delay
                              #'languagetool-server--start-check))

        ;; Start correction in changes
        (add-hook 'after-change-functions #'languagetool-server--check-on-change nil 'local)
        ;; Start correction on hooks
        (dolist (hook (reverse languagetool-server-delayed-commands))
          (add-hook hook #'languagetool-server-check nil 'local)))

    (progn
      ;; Delete active server checking timer if still active
      (when languagetool-server--start-check-timer
        (cancel-timer languagetool-server--start-check-timer))

      ;; Delete all the checking hooks
      (dolist (hook languagetool-server-delayed-commands)
        (remove-hook hook #'languagetool-server-check 'local))
      ;; Delete correction in changes
      (remove-hook 'after-change-functions #'languagetool-server--check-on-change 'local)

      ;; Delete all LanguageTool overlays
      (languagetool-server--clear-buffer))))

(defun languagetool-server--start-check ()
  "Start checking for correct init of LanguageTool Server."
  (let ((buffer (get-buffer-create languagetool-server-output-buffer-name)))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (when (or languagetool-server--started-p
                  (search-forward "Server started" nil t))
          (setq languagetool-server--started-p t)
          (when languagetool-server--start-check-timer
            (cancel-timer languagetool-server--start-check-timer))))))
  (languagetool-server-check))

(defun languagetool-server--check-on-change (_begin _end _len)
  "Correct the buffer using LanguageTool and show its suggestion.

This function checks for the actual showed region of the buffer
for suggestions.  This function is an alias to
`languagetool-server-check'.

_BEGIN, _END and _LEN are unused as the buffer needs to be check
completely."
  (languagetool-server-check))

(defun languagetool-server-check ()
  "Correct the buffer using LanguageTool and show its suggestion.

This function checks for the actual showed region of the buffer
for suggestions."
  (unless languagetool-server--correcting-p
    (request
      (format "%s:%d/v2/check" languagetool-server-url languagetool-server-port)
      :type "POST"
      :data (languagetool-server--parse-args)
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (languagetool-server--show-corrections (request-response-data response))))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (languagetool-server-mode -1)
                (message "LanguageTool got error: %S" error-thrown))))))

(defun languagetool-server--parse-args ()
  "Return the server argument list.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
  (let ((arguments (json-new-object)))

    ;; Appends the correction language information
    (setq arguments (json-add-to-object arguments "language" languagetool-default-language))

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (setq arguments (json-add-to-object arguments "motherTongue" languagetool-mother-tongue)))

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
        (setq arguments (json-add-to-object arguments "disabledRules" rules))))

    ;; Add the buffer contents
    (setq arguments (json-add-to-object arguments "text"
                                        (buffer-substring-no-properties (point-min) (point-max))))
    arguments))

(defun languagetool-server--show-corrections (json-parsed)
  "Show the suggestions made by LanguageTool in the buffer.

JSON-PARSED is a json object with the suggestions thrown by the
LanguageTool Server."
  (languagetool-server--clear-buffer)
  (let ((corrections (cdr (assoc 'matches json-parsed)))
        (correction nil))
    (dotimes (index (length corrections))
      (setq correction (aref corrections index))
      (let ((offset (cdr (assoc 'offset correction)))
            (size (cdr (assoc 'length correction))))
        (languagetool--create-overlay
         (+ (point-min) offset) (+ (point-min) offset size)
         correction)))))

(defun languagetool-server--clear-buffer ()
  "Deletes all the buffer overlays."
  (save-restriction
    (widen)
    (save-excursion
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'languagetool-message)
          (delete-overlay ov))))))

(provide 'languagetool-server)

;;; languagetool-server.el ends here
