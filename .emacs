(require 'package) ;; load the package manager
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; ergoemacs
(require 'ergoemacs-mode)

(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(setq ergoemacs-theme-options (quote ((save-options-on-exit off))))
(ergoemacs-mode 1)

;; Commands completion
(require 'smex)

(smex-initialize)
(global-set-key (kbd "M-a") 'smex)
(global-set-key (kbd "M-A") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-a") 'execute-extended-command) ; This is your old M-x.

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

;; http://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)

(defun colortheme-hook ()
  (if (display-graphic-p)
      (color-theme-emacs-21)
      (color-theme-pok-wob)
      ))

(colortheme-hook)

(add-hook 'server-visit-hook 'colortheme-hook)

(require 'back-button)

;;; Org-mode
(require 'org)

;;;; Autolist
(add-hook 'org-mode-hook (lambda () (org-autolist-mode)))

;;;; Update images from babel code blocks automatically
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;;;; Enable common programming language support in org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (python . t)
   (emacs-lisp . t)
   (latex . t)
   ))

;;;; Disable confirmation for code evaluation
(setq org-confirm-babel-evaluate nil)

;;;; Set sensible mode for editing dot files
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

;;;; Fontify code blocks in org-mode
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

;; Docker
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
(setq tramp-ssh-controlmaster-options nil)
(require 'tramp)
(require 'ssh)
(setq tramp-default-method "ssh")

(require 'ess-site)
(ess-toggle-S-assign nil)
(global-set-key (kbd "M--")  (lambda () (interactive) (insert " <- ")))
(setq ess-default-style 'RStudio)
(ess-toggle-underscore nil)             ; disable 'smart underscore'

(setq comint-scroll-to-bottom-on-input t) ;;;; Scroll down when R generates output
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(define-key ess-mode-map (kbd "<C-return>") 'ess-eval-line-and-step)
(define-key ess-mode-map (kbd "<C-S-return>") 'rmd-send-chunk)
(define-key ess-mode-map (kbd "C-S-i") 'rmd-new-chunk)
;; (define-key poly-markdown+r-mode-map (kbd "C-S-i") 'rmd-new-chunk)
;; (define-key markdown-mode-map (kbd "C-S-i") 'rmd-new-chunk)



;; put cursor in last used position when re-opening file
(require 'saveplace)
(setq-default save-place t)

;; Code navigation
(require 'imenu)
(setq imenu-auto-rescan t) ;; autoatically update functions list in the buffer
(global-set-key (kbd "C-.") #'imenu-anywhere)
(global-set-key (kbd "C-.") 'imenu)

;; Projects
(require 'projectile)
(require 'projectile-speedbar)

;; Folding
(defvar hs-special-modes-alist
   (mapcar 'purecopy
   '((c-mode "{" "}" "/[*/]" nil nil)
    (c++-mode "{" "}" "/[*/]" nil nil)
    (bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
    (java-mode "{" "}" "/[*/]" nil nil)
    (js-mode "{" "}" "/[*/]" nil)
    (r-mode "{" "}" nil nil)
    (emacs-lisp- "(" ")" nil))))

(require 'hideshow)

(global-set-key (kbd "<f9>") 'hs-toggle-hiding)
(global-set-key (kbd "C-<f9>") 'hs-hide-all)
(global-set-key (kbd "C-S-<f9>") 'hs-show-all)

;; Keybindings
(global-set-key (kbd "C-e") 'other-window)
(global-set-key (kbd "C-w") 'delete-window)
(global-set-key (kbd "C-M-w") 'kill-this-buffer)
(global-set-key (kbd "M-b") 'bs-show)
(global-set-key (kbd "C-M-e") 'split-window-right)
(global-set-key (kbd "C-M-h") 'split-window-below)
(global-set-key (kbd "M-2") 'execute-extended-command)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-1") 'abort-recursive-edit)
(global-set-key (kbd "M-f") 'isearch-forward-symbol-at-point)

;;;; Search
(define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-backward)

;;;; Org-mode
(defun emphasize-org-strikethrough ()
  (interactive)
  (org-emphasize ?+)
  )

(global-unset-key (kbd "C-y"))
(progn
  (require 'org)
;;  (define-key org-mode-map (kbd "<C-M-return>") 'org-ctrl-c-ctrl-c)
;;  (define-key org-mode-map (kbd "C-E") 'other-window)
  (define-key org-mode-map (kbd "M-a") 'smex)
  (define-key org-mode-map (kbd "<C-M-return>") 'org-insert-todo-heading)
  (define-key org-mode-map (kbd "C-t") 'org-todo)
  (define-key org-mode-map (kbd "<C-M-t>") 'org-ctrl-c-ctrl-c)
;;  (define-key org-mode-map (kbd "C-i") 'ergoemacs-org-italic)
  (define-key org-mode-map (kbd "C-y") 'emphasize-org-strikethrough)
  (define-key org-mode-map (kbd "M-q") 'org-fill-paragraph) )

(defun toggle-comment-on-line ()
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-_") 'toggle-comment-on-line)

;; Common settings
(add-hook 'text-mode-hook ; enable on-the-fly spell checking
          (lambda ()
            (flyspell-mode 1)))

(add-hook 'prog-mode-hook ; prevent flyspell from finding mistakes in the code
          (lambda ()
            ;; `ispell-comments-and-strings'
            (flyspell-prog-mode)))

(setq show-paren-style 'expression)
(show-paren-mode 2)

;; (menu-bar-mode -1) ;; remove toolbars
(tool-bar-mode -1)

(setq frame-title-format "GNU Emacs: %b")
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)
(electric-pair-mode 1) ;; close brackets

(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default nil) ; Don't want any auto saving

(setq scroll-step              1) ;; scroll by 1 line
(setq scroll-margin            5) ;; scroll when the cursor is near the border
(setq scroll-conservatively 10000)

(setq-default fill-column 160)

(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))

(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

(defalias 'yes-or-no-p 'y-or-n-p) ;; short messages

(setq x-select-enable-clipboard t ;; Make sure copy-and-paste works with other programs
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)


(setq search-highlight        t) ;; Highlight search resaults
(setq query-replace-highlight t)


(if (equal nil (equal major-mode 'org-mode)) ;; Easy transition between buffers: M-arrow-keys
    (windmove-default-keybindings 'meta))

(add-to-list 'write-file-functions 'delete-trailing-whitespace) ;; delete trailing whitespaces on save

(setq global-font-lock-mode 1) ; everything should use fonts
(setq font-lock-maximum-decoration t) ;; decorate as much as possible
(show-paren-mode t) ;; highlight matching paren

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
