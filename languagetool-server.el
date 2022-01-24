;;; languagetool-server.el --- Description -*- lexical-binding: t; -*-

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

;; LanguageTool commands and variables to use LanguageTool via
;; languagetool-server.jar or org.languagetool.server.HTTPServer.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'request)
(require 'languagetool-java)
(require 'languagetool-core)
(require 'languagetool-issue)

;; Group definition:

(defgroup languagetool-server nil
  "Real time LanguageTool Server"
  :tag "Server"
  :prefix "languagetool-server-"
  :group 'languagetool)

;; Variable definitions:

(defcustom languagetool-server-command nil
  "LanguageTool Server path or class.

When using LanguageTool you should set this variable to either
the path or the class to call LanguageTool."
  :group 'languagetool-server
  :type '(choice
          file
          string))

(defcustom languagetool-server-arguments nil
  "LanguageTool Server extra arguments.

More info at http://wiki.languagetool.org/command-line-options."
  :group 'languagetool-server
  :type '(choice
          (const nil)
          (repeat string)))

(defcustom languagetool-server-url "http://localhost"
  "LanguageTool Server host URL."
  :group 'languagetool-server
  :type 'string)

(defcustom languagetool-server-port 8081
  "LanguageTool Server host port."
  :group 'languagetool-server
  :type 'integer)

(defcustom languagetool-server-max-tries 20
  "LanguageTool Server max number of tries before disconnect from server."
  :group 'languagetool-server
  :type 'integer)

(defcustom languagetool-server-check-delay 3.0
  "LanguageTool Server delay time before checking again for issues."
  :group 'languagetool-server
  :type 'number)

(defvar languagetool-server-output-buffer-name "*LanguageTool Server Output*"
  "LanguageTool Server output buffer for debugging.")

(defvar languagetool-server-process nil
  "LanguageTool Server inferior process reference if any.")

(defvar-local languagetool-server-check-timer nil
  "Hold idle time that send request to LanguageTool server.")

(defvar languagetool-server-open-communication-p nil
  "Set to t if server communication is open, nil otherwise.")

(defvar languagetool-server-correction-p nil
  "Set to t if correcting errors, nil otherwise.")

;; Function definitions:

;;;###autoload
(define-minor-mode languagetool-server-mode
  "Toggle LanguageTool issue highlighting."
  :group 'languagetool-server
  :lighter " LT"
  (if languagetool-server-mode
      (languagetool-server-mode-on)
    (languagetool-server-mode-off)))


(defun languagetool-server-mode-on ()
  "Turn on LanguageTool Server mode.

Don't use this function, use `languagetool-server-mode' instead."
  ;; Start checking for LanguageTool server is able to handle requests
  (languagetool-server-check-for-communication)

  ;; Init checking timer
  (setq languagetool-server-check-timer
        (run-with-idle-timer languagetool-server-check-delay t
                             #'languagetool-server-check))

  ;; Init hint timer in the current buffer if not already
  (setq languagetool-core-hint-timer
        (run-with-idle-timer languagetool-hint-idle-delay t
                             languagetool-hint-function)))

(defun languagetool-server-mode-off ()
  "Turn off LanguageTool Server mode.

Don't use this function, use `languagetool-server-mode' instead."
  ;; Turn off buffer local flag for server communication.
  (setq languagetool-server-open-communication-p nil)

  ;; Cancel check timer
  (when (timerp languagetool-server-check-timer)
    (cancel-timer languagetool-server-check-timer))

  ;; Delete all LanguageTool overlays
  (languagetool-core-clear-buffer))


(defun languagetool-server-class-p ()
  "Return nil if `languagetool-server-command' is not a Java class."
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
    (string-match-p regex languagetool-server-command)))

(defun languagetool-server-command-exists-p ()
  "Return t is `languagetool-console-command' can be used or exists.

Also sets `languagetool-console-command' to a full path if needed
for this package to work."
  (or (languagetool-server-class-p)
      (when (file-readable-p (file-truename languagetool-server-command))
        (setq languagetool-server-command (file-truename languagetool-server-command))
        t)))

;;;###autoload
(defun languagetool-server-start ()
  "Start the LanguageTool Server.

It's not recommended to run this function more than once."
  (interactive)
  (unless (process-live-p languagetool-server-process)
    (unless (executable-find languagetool-java-bin)
      (error "Java could not be found"))
    (unless languagetool-server-command
      (error "LanguageTool Server Command is not set"))
    (unless (languagetool-server-command-exists-p)
      (error "LanguageTool Server Command could not be found"))

    (let ((buffer (get-buffer-create languagetool-server-output-buffer-name)))
      ;; Clean the buffer before printing the LanguageTool Server Log
      (with-current-buffer buffer
        (erase-buffer))

      ;; Start LanguageTool Server
      (setq languagetool-server-process
            (apply #'start-process
                   "*LanguageTool Server*"
                   buffer
                   languagetool-java-bin
                   (append
                    (languagetool-java-parse-arguments)
                    (languagetool-server-parse-arguments))))

      ;; Does not block Emacs when close and do not shutdown the server
      (set-process-query-on-exit-flag languagetool-server-process nil))

    ;; Start running the hint idle timer
    (setq languagetool-core-hint-timer
          (run-with-idle-timer languagetool-hint-idle-delay t
                               languagetool-hint-function))))

(defun languagetool-server-parse-arguments ()
  "Parse the arguments needed to start HTTP server."
  (unless (listp languagetool-server-arguments)
    (error "LanguageTool Server Arguments must be a list of strings"))

  (let ((arguments nil))

    ;; Appends the LanguageTool Server Command
    (unless (languagetool-server-class-p)
      (setq arguments (append arguments (list "-cp"))))
    (setq arguments (append arguments (list languagetool-server-command)))
    (unless (languagetool-server-class-p)
      (setq arguments (append arguments (list "org.languagetool.server.HTTPServer"))))

    (setq arguments (append arguments languagetool-server-arguments))

    ;; Appends the port information
    (setq arguments (append arguments
                            (list "--port"
                                  (format "%d" languagetool-server-port))))

    arguments))

;;;###autoload
(defun languagetool-server-stop ()
  "Stops the LanguageTool Server."
  (interactive)
  (delete-process languagetool-server-process))

(defun languagetool-server-check-for-communication (&optional try)
  "Check if the LanguageTool Server is able to handle requests.

Optional parameter TRY is the try number before Emacs show an error."
  (unless try (setq try 1))
  (when (>= try languagetool-server-max-tries)
    (languagetool-server-mode -1)
    (error "LanguageTool Server cannot communicate with server"))
  (unless languagetool-server-open-communication-p
    (request
      (format "%s:%d/v2/languages" languagetool-server-url languagetool-server-port)
      :type "GET"
      :parser 'json-read
      :success (cl-function
                (lambda (&key _response &allow-other-keys)
                  (message "LanguageTool Server communication is up...")
                  (setq languagetool-server-open-communication-p t)
                  (languagetool-server-check)))
      :error (cl-function
              (lambda (&key _error-thrown &allow-other-keys)
                (when (or
                       languagetool-server-mode
                       (not languagetool-server-open-communication-p))
                  (languagetool-server-check-for-communication (+ try 1))))))))

(defun languagetool-server-parse-request ()
  "Return a json-like object with LanguageTool Server request arguments parsed.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
  (let ((arguments (json-new-object)))

    ;; Appends the correction language information
    (setq arguments (json-add-to-object arguments
                                        "language"
                                        languagetool-default-language))

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (setq arguments (json-add-to-object arguments
                                          "motherTongue"
                                          languagetool-mother-tongue)))

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
    (setq arguments (json-add-to-object arguments
                                        "text"
                                        (buffer-substring-no-properties
                                         (point-min)
                                         (point-max))))
    arguments))

(defun languagetool-server-check ()
  "Show LanguageTool Server suggestions in the buffer.

This function checks for the actual showed region of the buffer
for suggestions."
  (when (and languagetool-server-mode
             (not languagetool-server-correction-p))
    (request
      (format "%s:%d/v2/check" languagetool-server-url languagetool-server-port)
      :type "POST"
      :data (languagetool-server-parse-request)
      :parser 'json-read
      :success (cl-function
                (lambda (&key response &allow-other-keys)
                  (languagetool-server-highlight-matches
                   (request-response-data response))))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (languagetool-server-mode -1)
                (error
                 "[Fatal Error] LanguageTool closed and got error: %S"
                 error-thrown))))))

(defun languagetool-server-highlight-matches (json-parsed)
  "Highlight LanguageTool Server issues in the buffer.

JSON-PARSED is a json object with the suggestions thrown by the
LanguageTool Server."
  (languagetool-core-clear-buffer)
  (when languagetool-server-mode
    (let ((corrections (cdr (assoc 'matches json-parsed)))
          (correction nil))
      (dotimes (index (length corrections))
        (setq correction (aref corrections index))
        (let ((offset (cdr (assoc 'offset correction)))
              (size (cdr (assoc 'length correction))))
          (languagetool-issue-create-overlay
           (+ (point-min) offset) (+ (point-min) offset size)
           correction))))))

(provide 'languagetool-server)

;;; languagetool-server.el ends here
