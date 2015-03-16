;;; helm-github-stars.el --- Helm integration for your starred repositories on github
;;
;; Author: Sliim <sliim@mailoo.org>
;; URL: https://github.com/Sliim/helm-github-stars
;; Version: 1.1.1
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
;;
;; At the first execution of ~helm-github-stars~, list of repositories is
;; fetched from github and saved into a cache file.
;; Default cache location: ~$HOME/.emacs.d/hgs-cache~.
;; To refresh cache and open helm interface run ~helm-github-stars-fetch~.
;;
;; You can customize cache file path:
;; (setq helm-github-stars-cache-file "/cache/path")

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

(defgroup helm-github-stars nil
  "Helm integration for your starred repositories on github."
  :group 'helm
  :prefix "helm-github-stars-")

(defcustom helm-github-stars-username "Sliim"
  "Github's username to fetch starred repositories."
  :type 'string)

(defcustom helm-github-stars-cache-file (concat user-emacs-directory "hgs-cache")
  "Cache file for starred repositories."
  :type 'string)

(defvar hgs/github-url "https://github.com/"
  "Github URL for browsing.")

(defvar hgs/helm-c-source-stars
  (helm-build-in-buffer-source "Starred repositories"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity (hgs/get-github-stars) "\n"))))
    :action (lambda (candidate)
              (let ((repo (substring candidate 0 (string-match " - " candidate))))
                (browse-url (concat hgs/github-url repo)))))
  "Helm source definition.")

(defvar hgs/helm-c-source-repos
  (helm-build-in-buffer-source "Your repositories"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity (hgs/get-github-repos) "\n"))))
    :action (lambda (candidate)
              (let ((repo (substring candidate 0 (string-match " - " candidate))))
                (browse-url (concat hgs/github-url repo)))))
  "Helm source definition.")

(defvar hgs/helm-c-source-search
  (helm-build-dummy-source "Search on github"
    :action (lambda (candidate)
              (browse-url (concat "https://github.com/search?q=" candidate)))))

(defun hgs/read-cache-file ()
  "Read cache file and return list of starred repositories."
  (with-temp-buffer
    (insert-file-contents helm-github-stars-cache-file)
    (read (current-buffer))))

(defun hgs/write-cache-file (hash)
  "Write HASH of repositories in cache file."
  (with-temp-buffer
    (let ((file helm-github-stars-cache-file)
          (coding-system-for-write 'utf-8))
      (print hash (current-buffer))
      (when (file-writable-p file)
        (write-region (point-min) (point-max) file)))))

(defun hgs/cache-file-exists ()
  "Check that cache file exists."
  (file-exists-p helm-github-stars-cache-file))

(defun hgs/clear-cache-file ()
  "Delete file cache if exists."
  (when (hgs/cache-file-exists)
    (delete-file helm-github-stars-cache-file)))

(defun hgs/generate-cache-file ()
  "Generate or regenerate cache file if already exists."
  (let ((stars-list [])
        (repos-list [])
        (cache-hash-table (make-hash-table :test 'equal)))
    ;; Fetch user's starred repositories
    (let ((next-request t)
          (current-page 1))
      (while next-request
        (let ((response (hgs/parse-github-response (hgs/request-github-stars current-page))))
          (if (= 0 (length response))
              (setq next-request nil)
            (progn
              (setq stars-list (vconcat stars-list response))
              (setq current-page (1+ current-page)))))))
    ;; Fetch user's repositories
    (let ((next-request t)
          (current-page 1))
      (while next-request
        (let ((response (hgs/parse-github-response (hgs/request-github-repos current-page))))
          (if (= 0 (length response))
              (setq next-request nil)
            (progn
              (setq repos-list (vconcat repos-list response))
              (setq current-page (1+ current-page)))))))

    (puthash '"stars" stars-list cache-hash-table)
    (puthash '"repos" repos-list cache-hash-table)
    (hgs/write-cache-file cache-hash-table)))

(defun hgs/request-github-stars (page)
  "Request Github API user's stars with PAGE parameter and return response."
  (hgs/request-github (concat "https://api.github.com/users/"
                              helm-github-stars-username
                              "/starred?per_page=100&page="
                              (number-to-string page))))

(defun hgs/request-github-repos (page)
  "Request Github API user's repositories with PAGE parameter and return response."
  (hgs/request-github (concat "https://api.github.com/users/"
                              helm-github-stars-username
                              "/repos?per_page=100&page="
                              (number-to-string page))))

(defun hgs/request-github (url)
  "Request Github URL and return response."
  (with-current-buffer
      (url-retrieve-synchronously url)
    (let ((start (save-excursion
                   (goto-char (point-min))
                   (and (re-search-forward "\\[" (point-max) t)
                        (match-beginning 0)))))
      (and start
           (decode-coding-string
            (buffer-substring-no-properties start (point-max))
            'utf-8)))))

(defun hgs/parse-github-response (response)
  "Parse Github API RESPONSE to get repositories full name."
  (let ((github-repos (json-read-from-string response))
        (repos [])
        (i 0))
    (while (< i (length github-repos))
      (setq repos (vconcat repos (vector (concat
                                          (cdr (assoc 'full_name (elt github-repos i)))
                                          " - "
                                          (cdr (assoc 'description (elt github-repos i)))))))
      (setq i (1+ i)))
    repos))

(defun hgs/get-github-stars ()
  "Get user's starred repositories."
  (when (not (hgs/cache-file-exists))
    (hgs/generate-cache-file))
  (gethash "stars" (hgs/read-cache-file)))

(defun hgs/get-github-repos ()
  "Get user's repositories."
  (when (not (hgs/cache-file-exists))
    (hgs/generate-cache-file))
  (gethash "repos" (hgs/read-cache-file)))

(defun helm-github-stars-fetch ()
  "Remove cache file before calling helm-github-stars."
  (interactive)
  (hgs/clear-cache-file)
  (helm-github-stars))

;;;###autoload
(defun helm-github-stars ()
  "Show and Browse your github's stars."
  (interactive)
  (helm :sources '(hgs/helm-c-source-stars
                   hgs/helm-c-source-repos
                   hgs/helm-c-source-search)
        :candidate-number-limit 9999
        :buffer "*hgs*"
        :prompt "> "))

(provide 'helm-github-stars)

;;; helm-github-stars.el ends here
