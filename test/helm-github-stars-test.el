;;; helm-github-stars-test.el --- Test suite for helm-github-stars.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0

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

(defvar github-stars-request-result-stub
  "[
  {
    \"full_name\": \"Sliim/helm-github-stars\",
    \"html_url\": \"https://github.com/Sliim/helm-github-stars\",
    \"description\": \"An Emacs helm interface for github user's stars.\",
    \"language\": \"Emacs Lisp\"
  },
  {
    \"full_name\": \"foo/awesome-project\",
    \"html_url\": \"https://github.com/foo/awesome-project\",
    \"description\": \"Wow An Amewome Project !!.\",
    \"language\": \"Python\"
  }
]")

(defun hgs/fetch-github-stars ()
  "Stub function."
  github-stars-request-result-stub)

(ert-deftest hgs/github-stars-list-test ()
  (should (equal (hgs/github-stars-list)
                 '("Sliim/helm-github-stars"
                   "foo/awesome-project"))))

;;; helm-github-stars-test.el ends here
