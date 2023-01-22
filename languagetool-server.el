;;; languagetool-server.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2022  Joar Buitrago

;; Author: Joar Buitrago <jebuitragoc@unal.edu.co>
;; Keywords: grammar text docs tools convenience checker
;; URL: https://github.com/PillFall/Emacs-LanguageTool.el
;; Version: 1.2.0
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

;; LanguageTool commands and variables to use LanguageTool via
;; languagetool-server.jar or org.languagetool.server.HTTPServer.

;;; Code:

(require 'json)
(require 'url)
(require 'languagetool-java)
(require 'languagetool-core)
(require 'languagetool-issue)

;; Group definition:

(defgroup languagetool-server nil
  "Real time LanguageTool Server."
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

(defcustom languagetool-server-max-timeout 5.0
  "LanguageTool Server maximum number of seconds to wait before giving up."
  :group 'languagetool-server
  :type 'number)

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
  "Return non-nil if `languagetool-server-command' is a Java class."
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
  "Return non-nil is `languagetool-console-command' can be used or exists.

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

  (let (arguments)

    ;; Appends the LanguageTool Server Command
    (unless (languagetool-server-class-p)
      (push "-cp" arguments))
    (push languagetool-server-command arguments)
    (unless (languagetool-server-class-p)
      (push "org.languagetool.server.HTTPServer" arguments))

    (push languagetool-server-arguments arguments)

    ;; Appends the port information
    (push (list "--port" (format "%d" languagetool-server-port)) arguments)

    (flatten-tree (reverse arguments))))

;;;###autoload
(defun languagetool-server-stop ()
  "Stops the LanguageTool Server."
  (interactive)
  (delete-process languagetool-server-process))

(defun languagetool-server-check-for-communication ()
  "Check if the LanguageTool Server is able to handle requests.

This methods will only check if the server is up for the number
of seconds specified in `languagetool-server-max-timeout'."
  (unless languagetool-server-open-communication-p
    (condition-case nil
        (let ((url-request-method "GET"))
          (with-current-buffer (url-retrieve-synchronously
                                (url-encode-url (format "%s:%d/v2/languages" languagetool-server-url languagetool-server-port))
                                nil
                                nil
                                languagetool-server-max-timeout)
            (when (/= (symbol-value 'url-http-response-status) 200)
              (error "Not successful response"))
            (setq languagetool-server-open-communication-p t)
            (message "LanguageTool Server communication is up...")
            (languagetool-server-check)))
      (error
       (languagetool-server-mode -1)
       (error "LanguageTool Server cannot communicate with server")))))

(defun languagetool-server-parse-request ()
  "Return a assoc-list with LanguageTool Server request arguments parsed.

Return the arguments as an assoc list of string which will be
used in the POST request made to the LanguageTool server."
  (let (arguments)

    ;; Appends the correction language information
    (push (list "language" languagetool-correction-language) arguments)

    ;; Appends the mother tongue information
    (when (stringp languagetool-mother-tongue)
      (push (list "motherTongue" languagetool-mother-tongue) arguments))

    ;; Add LanguageTool Preamium features
    (when (stringp languagetool-api-key)
      (push (list "apiKey" languagetool-api-key) arguments))

    (when (stringp languagetool-username)
      (push (list "username" languagetool-username) arguments))

    ;; Appends the disabled rules
    (let ((rules))
      ;; Global disabled rules
      (setq rules (string-join (append languagetool-disabled-rules languagetool-local-disabled-rules) ","))
      (unless (string= rules "")
        (push (list "disabledRules" rules) arguments)))

    ;; Add the buffer contents
    (push (list "text" (buffer-substring-no-properties (point-min) (point-max))) arguments)))

(defun languagetool-server-check ()
  "Show LanguageTool Server suggestions in the buffer.

This function checks for the actual showed region of the buffer
for suggestions."
  (when (and languagetool-server-mode
             (not languagetool-server-correction-p))
    (let ((url-request-method "POST")
          (url-request-data (url-build-query-string (languagetool-server-parse-request))))
      (url-retrieve
       (url-encode-url(format "%s:%d/v2/check" languagetool-server-url languagetool-server-port))
       #'languagetool-server-highlight-matches
       (list (current-buffer))
       t))))

(defun languagetool-server-highlight-matches (_status checking-buffer)
  "Highlight LanguageTool Server issues in CHECKING-BUFFER.

STATUS is a plist thrown by Emacs url. Throws an error if the response is null."
  (when (/= (symbol-value 'url-http-response-status) 200)
    (error "LanguageTool Server closed"))
  (set-buffer-multibyte t)
  (goto-char (point-max))
  (backward-sexp)
  (let ((json-parsed (json-read)))
    (with-current-buffer checking-buffer
      (save-excursion
        (languagetool-core-clear-buffer)
        (when languagetool-server-mode
          (let ((corrections (alist-get 'matches json-parsed)))
            (dotimes (index (length corrections))
              (let* ((correction (aref corrections index))
                     (offset (alist-get 'offset correction))
                     (size (alist-get 'length correction))
                     (start (+ (point-min) offset))
                     (end (+ (point-min) offset size))
                     (word (buffer-substring-no-properties start end)))
                (unless (languagetool-core-correct-p word)
                  (languagetool-issue-create-overlay start end correction))))))))))

(provide 'languagetool-server)

;;; languagetool-server.el ends here
