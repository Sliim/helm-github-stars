(defvar emacsd-dir (file-name-directory load-file-name)
  "The root dir of project.")
(defvar emacsd-cask-dir (expand-file-name ".cask" emacsd-dir)
  "Cask root directory.")
(defvar emacsd-elpa-dir (expand-file-name
                         (concat (number-to-string emacs-major-version) "."
                                 (number-to-string emacs-minor-version) "/elpa")
                         emacsd-cask-dir)
  "Elpa packages root directory.")
(defvar emacsd-var-dir (expand-file-name "var" emacsd-dir)
  "Tmp var dir.")
(defvar emacsd-cache-file (expand-file-name "hgs-cache" emacsd-var-dir)
  "Cache file.")

(unless (file-exists-p emacsd-var-dir)
  (make-directory emacsd-var-dir))

(let ((default-directory emacsd-elpa-dir))
    (normal-top-level-add-subdirs-to-load-path))

(setq helm-github-stars-cache-file emacsd-cache-file
      helm-github-stars-username "Sliim" ;; Warning! Too many stars for unauthenticated requests, change this or you will hit the API rate limit
      )

(load-file "helm-github-stars.el")
(load-file "~/.emacs.d/modules/emacsd-tls-hardening-module.el")
(require 'helm-github-stars)
(helm-github-stars-fetch)
