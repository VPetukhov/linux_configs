;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package) ;; You might already have this line
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(require 'ergoemacs-mode)

(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(setq ergoemacs-theme-options (quote ((save-options-on-exit off))))
(ergoemacs-mode 1)

;; Other settings
(setq show-paren-style 'expression)
(show-paren-mode 2)

(menu-bar-mode -1) ;; remove toolbars
(tool-bar-mode -1)


(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default nil) ; Don't want any auto saving

(add-to-list 'load-path "~/.emacs.d/addons/")
(add-to-list 'load-path "~/.emacs.d/auto-complete/")

;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/linum%2B.el
(require 'linum+)
(setq linum-format "%d ")
(global-linum-mode 1)

;; built-in
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; built-in
(require 'bs)
(setq bs-configurations
      '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))

;; http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "/home/dim/EmacsCasts/episode03/auto-complete/dict")

;; http://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

(require 'yasnippet)
(yas-global-mode 1)

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(defun colortheme-hook ()
  (if (display-graphic-p)
      (color-theme-emacs-21)
      (color-theme-pok-wob)
      ))

(add-hook 'server-visit-hook 'colortheme-hook)

;; Docker
(require 'docker)
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Polymode
;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(require 'markdown-mode)
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; ESS
(require 'ess-site)
(require 'tramp)
(setq tramp-default-method "ssh")
(require 'ssh)
(ess-toggle-S-assign nil)
(global-set-key (kbd "M--")  (lambda () (interactive) (insert " <- ")))
(setq ess-default-style 'RStudio)

;; Code navigation
(global-set-key (kbd "C-.") #'imenu-anywhere)


;; Keybindings
(global-set-key (kbd "C-e") 'other-window)
(global-set-key (kbd "C-w") 'delete-window)
(global-set-key (kbd "C-M-w") 'kill-this-buffer)
(global-set-key (kbd "C-b") 'bs-show)
(global-set-key (kbd "C-M-e") 'split-window-right)
(global-set-key (kbd "C-M-h") 'split-window-below)
(global-set-key (kbd "M-2") 'execute-extended-command)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-1") 'abort-recursive-edit)
(global-set-key (kbd "C-p") 'ess-eval-line)
(global-set-key (kbd "C-M-p") 'rmd-send-chunk)
(global-set-key (kbd "M-ESC") 'rmd-new-chunk)

(defun toggle-comment-on-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-_") 'toggle-comment-on-line)

;; Helper functions
(defun rada()
       (interactive)
       (ssh "-Y -C -t ada 'R'"))

(eval-when-compile
  (require 'polymode-core)
  (defvar pm/chunkmode))
(declare-function pm-map-over-spans "polymode-core")
(declare-function pm-narrow-to-span "polymode-core")

(defun rmd-new-chunk ()
  "Insert a new R chunk."
  (interactive)
  (insert "\n```{r}\n")
  (save-excursion
    (newline)
    (insert "```\n")
    (previous-line)))

(defun rmd-send-chunk ()
  "Send current R chunk to ess process."
  (interactive)
  (and (eq (oref pm/chunkmode :mode) 'r-mode) ;;'
       (pm-with-narrowed-to-span nil
	 (goto-char (point-min))
	 (forward-line)
	 (ess-eval-region (point) (point-max) nil nil 'R))))

(defun rmd-send-buffer (arg)
    "Send all R code blocks in buffer to ess process. With prefix
send regions above point."
    (interactive "P")
    (save-restriction
      (widen)
      (save-excursion
	(pm-map-over-spans
	 'rmd-send-chunk (point-min) ;;'
	 ;; adjust this point to send prior regions
	        (if arg (point) (point-max))))))

(defun dev-load()
  (interactive)
  (ess-eval-linewise "devtools::load_all()"))
