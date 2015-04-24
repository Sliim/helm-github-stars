;;; helm-github-stars.el --- Helm integration for your starred repositories on github
;;
;; Author: Sliim <sliim@mailoo.org>
;;    xuchunyang <xuchunyang56@gmail.com>
;; URL: https://github.com/Sliim/helm-github-stars
;; Version: 1.2.1
;; Package-Requires: ((helm "1.6.8") (emacs "24.4"))
;; Keywords: helm github stars

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; helm-github-stars provides capabilities to fetch your starred
;; repositories from github and select one for browsing.
;;
;; Install:
;;
;;     M-x package-install helm-github-stars
;;
;; Usage:
;;
;; Copy helm-github-stars.el in your load-path and put this in your ~/.emacs.d/init.el:
;;
;;     (require 'helm-github-stars)
;;     ;; Setup your github username:
;;     (setq helm-github-stars-username "USERNAME")
;;
;; Type M-x helm-github-stars to show starred repositories.
;;
;; At the first execution of helm-github-stars, list of repositories is
;; fetched from github and saved into a cache file.
;; Default cache location: $HOME/.emacs.d/hgs-cache.
;; To refresh cache and open helm interface run helm-github-stars-fetch.
;;
;; You can customize cache file path:
;;
;;     (setq helm-github-stars-cache-file "/cache/path")
;;
;; For a clean look, repositories's description is aligned by default, you can
;; customize this behavior via helm-github-stars-name-length, it's default
;; value is 30.
;; You can disable this by setting helm-github-stars-name-length to nil:
;;
;;     (setq helm-github-stars-name-length nil)
;;
;; If you want to be able to show your private repositories, customize
;; helm-github-stars-token:
;;
;;     (setq helm-github-stars-token "TOKEN")

;;; Code:

;; requires
(require 'helm)
(require 'helm-utils)
(require 'json)
(require 'subr-x)

(defgroup helm-github-stars nil
  "Helm integration for your starred repositories on github."
  :group 'helm
  :prefix "helm-github-stars-")

(defcustom helm-github-stars-username "Sliim"
  "Github's username to fetch starred repositories."
  :type 'string)

(defcustom helm-github-stars-token nil
  "Access token to use for your repositories and your starred repositories.

If you don't have or don't want to show your private repositories, you don't
need access token at all.

To generate an access token:
  1. Visit the page https://github.com/settings/tokens/new and
     login to github (if asked).
  2. Give the token any name you want (helm-github-stars, for instance).
  3. The permission we need is \"repo\" and \"delete_repo\", so unmark
     all others.
  4. Click on \"Generate Token\", copy the generated token, and
     save it to this variable by writing
         (setq helm-github-stars-token TOKEN)
     somewhere in your configuration and evaluating it (or just
     restart emacs).

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account.
END DISCLAIMER

when disabled (nil) don't use Github token."
  :type '(choice (string :tag "Token")
                 (const :tag "Disable" nil)))

(defcustom helm-github-stars-cache-file (concat user-emacs-directory "hgs-cache")
  "Cache file for starred repositories."
  :type 'file)

(defcustom helm-github-stars-name-length 30
  "Length of repo name before truncate.
When disabled (nil) don't align description."
  :type  '(choice (const :tag "Disabled" nil)
                  (integer :tag "Length before truncate")))

(defcustom helm-github-stars-refetch-time nil
  "Days to refetch cache file.
When disabled (nil) don't refetch automatically. "
  :type '(choice (const :tag "Disabled" nil)
                 (number :tag "Days to refetch cache file")))

(defvar hgs/github-url "https://github.com/"
  "Github URL for browsing.")

(defvar hgs/helm-stars-actions
  (helm-make-actions
   "Browse URL"
   (lambda (candidate)
     (browse-url (concat hgs/github-url (hgs/get-repo-name candidate))))
   "Show URL"
   (lambda (candidate)
     (message (concat hgs/github-url (hgs/get-repo-name candidate))))
   "Clone"
   #'hgs/clone
   "Unstar"
   (lambda (candidate)
     "Unstar a starred repository."
     (let ((repo-name (hgs/get-repo-name candidate)))
       (hgs/unstar-or-delete-repo "https://api.github.com/user/starred/" repo-name)))))

(defvar hgs/helm-repos-actions
  (helm-make-actions
   "Browse URL"
   (lambda (candidate)
     (browse-url (concat hgs/github-url (hgs/get-repo-name candidate))))
   "Show URL"
   (lambda (candidate)
     (message (concat hgs/github-url (hgs/get-repo-name candidate))))
   "Clone"
   #'hgs/clone
   "Delete"
   (lambda (candidate)
     "Delete a user repository."
     (let ((repo-name (hgs/get-repo-name candidate)))
       (hgs/unstar-or-delete-repo "https://api.github.com/repos/" repo-name)))))

(defvar hgs/helm-c-source-stars
  (helm-build-in-buffer-source "Starred repositories"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity (hgs/get-github-stars) "\n"))))
    :real-to-display (lambda (candidate) (hgs/align-description candidate))
    :action hgs/helm-stars-actions)
  "Helm source definition.")

(defvar hgs/helm-c-source-repos
  (helm-build-in-buffer-source "Your repositories"
    :init (lambda ()
            (with-current-buffer (helm-candidate-buffer 'local)
              (insert (mapconcat 'identity (hgs/get-github-repos) "\n"))))
    :real-to-display (lambda (candidate) (hgs/align-description candidate))
    :action hgs/helm-repos-actions)
  "Helm source definition.")

(defvar hgs/helm-c-source-search
  (helm-build-dummy-source "Search on github"
    :action (lambda (candidate)
              (browse-url (concat "https://github.com/search?q=" candidate)))))

(defun hgs/align-description (item)
  "Truncate repo name in ITEM."
  (let* ((index (string-match " - " item))
         (name (substring item 0 index))
         (description (substring item (+ 3 index))))
    (concat
     ;; Name
     (if (> (string-width name) helm-github-stars-name-length)
         (helm-substring-by-width name helm-github-stars-name-length)
       (concat name (make-string
                     (- (+ helm-github-stars-name-length 3)
                        (string-width name)) ? )))
     ;; Separator
     "   "
     ;; Description
     description)))

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

(defun hgs/clear-cache-file-by-time ()
  "Delete cache file if it is too old."
  (when (and (hgs/cache-file-exists) helm-github-stars-refetch-time)
    (let ((time-since-last-fetch
           (time-subtract (current-time)
                          (nth 5 (file-attributes helm-github-stars-cache-file)))))
      (when (> (time-to-number-of-days time-since-last-fetch)
               helm-github-stars-refetch-time)
        (hgs/clear-cache-file)))))

(defun hgs/generate-cache-file ()
  "Generate or regenerate cache file if already exists."
  (let ((stars-list [])
        (repos-list [])
        (cache-hash-table (make-hash-table :test 'equal)))
    ;; Fetch user's starred repositories
    (let ((next-request t)
          (current-page 1))
      (while next-request
        (let ((response (hgs/parse-github-response
                         (funcall (if helm-github-stars-token
                                      'hgs/request-github-stars-by-token
                                    'hgs/request-github-stars)
                                  current-page))))
          (if (= 0 (length response))
              (setq next-request nil)
            (progn
              (setq stars-list (vconcat stars-list response))
              (setq current-page (1+ current-page)))))))
    ;; Fetch user's repositories
    (let ((next-request t)
          (current-page 1))
      (while next-request
        (let ((response (hgs/parse-github-response
                         (funcall (if helm-github-stars-token
                                      'hgs/request-github-repos-by-token
                                    'hgs/request-github-repos)
                                  current-page))))
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

(defun hgs/request-github-repos-by-token (page)
  "Request Github API user's repositories and return response."
  (let ((url-request-extra-headers `(("Authorization" .
                                      ,(format "token %s" helm-github-stars-token)))))
    (hgs/request-github (concat
                         "https://api.github.com/user/repos?per_page=100&page="
                         (number-to-string page)))))

(defun hgs/request-github-stars-by-token (page)
  "Request Github API user's repositories and return response."
  (let ((url-request-extra-headers `(("Authorization" .
                                      ,(format "token %s" helm-github-stars-token)))))
    (hgs/request-github (concat
                         "https://api.github.com/user/starred?per_page=100&page="
                         (number-to-string page)))))

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

(defun hgs/unstar-or-delete-repo (api repo-name)
  "Unstar a starred repository or delete a user repository."
  (unless helm-github-stars-token
    (error "`helm-github-stars-token' is nil."))

  (let ((url-request-method "DELETE")
        (url-request-extra-headers
         `(("Authorization" . ,(format "token %s" helm-github-stars-token)))))
    (with-current-buffer (url-retrieve-synchronously (concat api repo-name))
      (goto-char (point-min))
      (when (not (string-match "204 No Content" (buffer-string)))
        (error "Problem unstar or delete repo."))
      (kill-buffer)))

  ;; FIXME: there is no way to modify the cache file
  ;; (puthash "stars" (delete candidate (gethash "stars" (hgs/read-cache-file)))
  ;;          (hgs/read-cache-file))
  ;; (hgs/write-cache-file cache-hash-table)
  (helm-github-stars-fetch))

(defun hgs/get-repo-name (candidate)
  (substring candidate 0 (string-match " - " candidate)))

(defun hgs/clone (candidate)
  (unless (executable-find "git") (error "git not found."))
  (let* ((repo-name (hgs/get-repo-name candidate))
         (repository (format "https://github.com/%s.git" repo-name))
         (default-clone-directory
           (substring repo-name (1+ (string-match "/" repo-name))))
         (directory
          (read-directory-name
           (format "Clone %s to: " repo-name) nil nil nil default-clone-directory))
         (command (format "git clone %s %s" repository directory))
         (output-buffer "*git-clone-output*")
         (ret (let ((progress-reporter
                     (make-progress-reporter (format "Running '%s'..." command)
                                             nil nil)))
                (prog1 (call-process-shell-command command nil output-buffer)
                  (progress-reporter-done progress-reporter)))))
    (if (zerop ret)
        (progn
          (kill-buffer output-buffer)
          (message "Git clone done."))
      (error "Git clone failed, see %s buffer for details." output-buffer))))


(defun helm-github-stars-fetch ()
  "Remove cache file before calling helm-github-stars."
  (interactive)
  (hgs/clear-cache-file)
  (helm-github-stars))

;;;###autoload
(defun helm-github-stars ()
  "Show and Browse your github's stars."
  (interactive)
  (hgs/clear-cache-file-by-time)
  (helm :sources '(hgs/helm-c-source-stars
                   hgs/helm-c-source-repos
                   hgs/helm-c-source-search)
        :candidate-number-limit 9999
        :buffer "*helm github stars*"
        :prompt "> "))

(provide 'helm-github-stars)

;;; helm-github-stars.el ends here
