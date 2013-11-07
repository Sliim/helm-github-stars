;;; helm-github-stars-test.el --- Test suite for helm-github-stars.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.2

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test suite for helm-github-stars.
;;
;; To run tests, install dependencies with Cask:
;; $ cask install
;;
;; And execute ert-runner:
;; $ cask exec ert-runner

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

(defvar github-stars-response-stub
  ["[
  {
    \"full_name\": \"Sliim/helm-github-stars\",
    \"html_url\": \"https://github.com/Sliim/helm-github-stars\",
    \"description\": \"An Emacs helm interface for github user's stars.\",
    \"language\": \"Emacs Lisp\"
  },{
    \"full_name\": \"foo/awesome-project\",
    \"html_url\": \"https://github.com/foo/awesome-project\",
    \"description\": \"Wow An Amewome Project !!.\",
    \"language\": \"Python\"
  }]" "[
  {
    \"full_name\": \"bar/another-project\",
    \"html_url\": \"https://github.com/bar/another-project\",
    \"description\": \"Another Project ...\",
    \"language\": \"Haskell\"
  }]" "[ ]"])

(ert-deftest hgs/read-cache-file-test ()
  (with-cache
   (f-write-text "foo,baz,bar" 'utf-8 cache-test-file)
   (should (equal (hgs/read-cache-file) '("foo" "baz" "bar")))))

(ert-deftest hgs/write-cache-file-test ()
  (with-cache
   (hgs/write-cache-file '("foo" "bar" "baz"))
   (should (equal (f-read-text cache-test-file) "foo,bar,baz"))))

(ert-deftest hgs/cache-file-exists-test ()
  (with-cache
   (f-touch cache-test-file)
   (should (hgs/cache-file-exists))
   (f-delete cache-test-file)
   (should (not (hgs/cache-file-exists)))))

(ert-deftest hgs/clear-cache-file-test ()
  (with-cache
   (f-touch cache-test-file)
   (hgs/clear-cache-file)
   (should (not (file-exists-p cache-test-file)))))

(ert-deftest hgs/generate-cache-file-test ()
  (with-cache
   (defun hgs/request-github-stars (page)
     "Stub github response. Return PAGE element."
     (elt github-stars-response-stub (1- page)))
   (hgs/generate-cache-file)
   (should (equal (f-read-text cache-test-file)
                  "Sliim/helm-github-stars,foo/awesome-project,bar/another-project"))))

(ert-deftest hgs/github-parse-github-response-test ()
  (should (equal (hgs/parse-github-response (elt github-stars-response-stub 0))
                 '("Sliim/helm-github-stars"
                   "foo/awesome-project"))))

(ert-deftest hgs/get-github-stars-with-generated-cache-test ()
  (with-cache
   (f-write-text "foo,bar,baz" 'utf-8 cache-test-file)
   (should (equal (hgs/get-github-stars) '("foo" "bar" "baz")))))

(ert-deftest hgs/get-github-stars-without-generated-cache-test ()
  (with-cache
   (defun hgs/request-github-stars (page)
     "Stub github response."
     (elt github-stars-response-stub (1- page)))
   (should (equal (hgs/get-github-stars) '("Sliim/helm-github-stars"
                                           "foo/awesome-project"
                                           "bar/another-project")))))

(ert-deftest helm-github-stars-fetch-test ()
  "This test just check that cache file is removed before calling helm-github-stars"
  (with-cache
   (defun helm-github-stars ()
     "Overwrite helm-github-stars. Return t if cache file exists, nil if not exists."
     (f-file? cache-test-file))
   (f-write-text "foo,baz,bar" 'utf-8 cache-test-file)
   (should (not (helm-github-stars-fetch)))))

;;; helm-github-stars-test.el ends here
