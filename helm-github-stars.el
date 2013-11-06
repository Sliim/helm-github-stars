;;; helm-github-stars.el --- Helm integration for your starred repositories on github
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.1
;; Package-Requires: ((helm "1.5.5"))
;; Keywords: helm github stars

;; This file is not part of GNU Emacs.

;;; Commentary:

;; helm-github-stars provides capabilities to show and open
;; starred repository from github.
;;
;; Usage:
;;  Copy helm-github-stars.el in your load-path and put this in your ~/.emacs.d/init.el:
;;  (require 'helm-github-stars)
;;  ;; Setup your github username:
;;  (setq helm-github-stars-username "USERNAME")
;;
;;  Type M-x helm-github-stars to show starred repositories.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; requires
(require 'helm)
(require 'json)

(defcustom helm-github-stars-username "Sliim"
  "Github's username to fetch starred repositories."
  :type 'string
  :group 'helm)

(defcustom helm-github-stars-cache-file (concat user-emacs-directory "hgs-cache")
  "Cache file for starred repositories."
  :type 'string
  :group 'helm)

(defvar hgs/github-url "https://github.com/"
  "Github URL for browsing.")

(defvar hgs/helm-c-source
  `((name . "Github stars")
    (disable-shortcuts)
    (init . (lambda ()
              (with-current-buffer (helm-candidate-buffer 'local)
                (insert
                 (s-join "\n" (hgs/get-github-stars))))))
    (candidates-in-buffer)
    (action . (lambda (candidate)
                (browse-url (concat hgs/github-url candidate)))))
  "Helm source definition.")

(defun hgs/read-cache-file ()
  "Read cache file and return list of starred repositories."
  (with-temp-buffer
    (insert-file-contents helm-github-stars-cache-file)
    (split-string (buffer-string) ",")))

(defun hgs/write-cache-file (list)
  "Write LIST of repositories in cache file."
  (with-temp-buffer
    (let ((file helm-github-stars-cache-file))
      (insert (mapconcat 'identity list ","))
      (when (file-writable-p file)
        (write-region (point-min) (point-max) file)))))

(defun hgs/cache-file-exists ()
  "Check that cache file exists."
  (file-exists-p helm-github-stars-cache-file))

(defun hgs/clear-cache-file ()
  "Delete file cache if exists."
  (when (hgs/cache-file-exists)
    (delete-file helm-github-stars-cache-file)))

(defun hgs/request-github-stars ()
  "Request Github API user's stars and return response."
  (with-current-buffer
      (url-retrieve-synchronously (concat "https://api.github.com/users/"
                                          helm-github-stars-username
                                          "/starred?per_page=100"))
    (let ((start (save-excursion
                   (goto-char (point-min))
                   (and (re-search-forward "\\[" (point-max) t)
                        (match-beginning 0)))))
      (and start
           (buffer-substring start (point-max))))))

(defun hgs/parse-github-response (response)
  "Parse Github API RESPONSE to get repositories full name."
  (setq stars '())
  (let ((github-stars (json-read-from-string response)))
    (setq i 0)
    (while (< i (length github-stars))
      (add-to-list 'stars (cdr (assoc 'full_name (elt github-stars i))) t)
      (setq i (1+ i))))
  stars)

(defun hgs/get-github-stars ()
  "Get github stars."
  (when (not (hgs/cache-file-exists))
    (helm-github-stars-generate-cache-file))
  (hgs/read-cache-file))

(defun helm-github-stars-generate-cache-file ()
  "Generate or regenerate cache file if already exists."
  (interactive)
  (hgs/write-cache-file (hgs/parse-github-response (hgs/request-github-stars))))

(defun helm-github-stars-fetch ()
  "Remove cache file before calling helm-github-stars."
  (interactive)
  (hgs/clear-cache-file)
  (helm-github-stars))

;;;###autoload
(defun helm-github-stars ()
  "Show and Browse your github's stars."
  (interactive)
  (helm :sources '(hgs/helm-c-source)
        :buffer "*hgs*"
        :prompt "> "))

(provide 'helm-github-stars)

;;; helm-github-stars.el ends here
