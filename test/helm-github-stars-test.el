;;; helm-github-stars-test.el --- Test suite for helm-github-stars.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.2.0

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

;; Test suite for helm-github-stars.
;;
;; To run tests, install dependencies with Cask:
;;
;;     $ cask install
;;
;; And execute ert-runner:
;;
;;     $ cask exec ert-runner

;;; Code:

(ert-deftest hgs/read-cache-file-test ()
  (with-cache
   (f-write-text hgs-test/cache-string 'utf-8 cache-test-file)
   (should (equal (gethash "repos" (hgs/read-cache-file)) hgs-test/repos-list))
   (should (equal (gethash "stars" (hgs/read-cache-file)) hgs-test/stars-list))))

(ert-deftest hgs/write-cache-file-test ()
  (with-cache
   (hgs/write-cache-file hgs-test/cache-hash-table)
   (should (equal (f-read-text cache-test-file)
                  hgs-test/cache-string))))

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
     (elt hgs-test/github-stars-response-stub (1- page)))
   (defun hgs/request-github-repos (page)
     "Stub github response. Return PAGE element."
     (elt hgs-test/github-repos-response-stub (1- page)))
   (hgs/generate-cache-file)
   (should (equal (f-read-text cache-test-file)
                  hgs-test/cache-string))))

(ert-deftest hgs/parse-github-response-test ()
  (should (equal (hgs/parse-github-response (elt hgs-test/github-stars-response-stub 0))
                 ["star/1 - desc-star1" "star/2 - desc-star2"])))

(ert-deftest hgs/get-github-stars-with-generated-cache-test ()
  (with-cache
   (f-write-text hgs-test/cache-string 'utf-8 cache-test-file)
   (should (equal (hgs/get-github-stars) ["star/1 - desc-star1" "star/2 - desc-star2" "star/3 - desc-star3"]))))

(ert-deftest hgs/get-github-stars-without-generated-cache-test ()
  (with-cache
   (defun hgs/request-github-stars (page)
     "Stub github response."
     (elt hgs-test/github-stars-response-stub (1- page)))
   (defun hgs/request-github-repos (page)
     "Stub github response."
     (elt hgs-test/github-repos-response-stub (1- page)))
   (should (equal (hgs/get-github-stars) ["star/1 - desc-star1" "star/2 - desc-star2" "star/3 - desc-star3"]))))

(ert-deftest hgs/get-github-repos-with-generated-cache-test ()
  (with-cache
   (f-write-text hgs-test/cache-string 'utf-8 cache-test-file)
   (should (equal (hgs/get-github-repos) ["repo/1 - desc-repo1" "repo/2 - desc-repo2" "repo/3 - desc-repo3"]))))

(ert-deftest hgs/get-github-repos-without-generated-cache-test ()
  (with-cache
   (defun hgs/request-github-stars (page)
     "Stub github response."
     (elt hgs-test/github-stars-response-stub (1- page)))
   (defun hgs/request-github-repos (page)
     "Stub github response."
     (elt hgs-test/github-repos-response-stub (1- page)))
   (should (equal (hgs/get-github-repos) ["repo/1 - desc-repo1" "repo/2 - desc-repo2" "repo/3 - desc-repo3"]))))

(ert-deftest helm-github-stars-fetch-test ()
  "This test just check that cache file is removed before calling helm-github-stars"
  (with-cache
   (defun helm-github-stars ()
     "Overwrite helm-github-stars. Return t if cache file exists, nil if not exists."
     (f-file? cache-test-file))
   (f-write-text hgs-test/cache-string 'utf-8 cache-test-file)
   (should (not (helm-github-stars-fetch)))))

(ert-deftest hgs/align-description-test ()
  (let ((helm-github-stars-name-length 7)
        (repo1 "repo/a - description")
        (repo2 "repo/bb - description")
        (repo3 "repo/ccc - description")
        (repo4 "repo/dddd - description"))
    (should (string-equal (hgs/align-description repo1) "repo/a       description"))
    (should (string-equal (hgs/align-description repo2) "repo/bb      description"))
    (should (string-equal (hgs/align-description repo3) "repo/cc...   description"))
    (should (string-equal (hgs/align-description repo4) "repo/dd...   description"))))

;;; helm-github-stars-test.el ends here
