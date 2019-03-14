(require 'package) ;; load the package manager
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))


(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)


;; Make a list of the packages to install
(setq package-list '(ergoemacs-mode
		     smex
		     yasnippet,
		     color-theme,
;;                     ido-ubiquitous
;;                     outline-magic
;;                     smooth-scroll
                     auto-complete
;;                     auctex
                     ess
;;                     org-plus-contrib
                     markdown-mode
                     polymode))


;; Activate package autoloads
(package-initialize)


;; Fetch the list of packages available
(when (not package-archive-contents)
  (package-refresh-contents))


;; Install packages in package-list if they are not already installed
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))
