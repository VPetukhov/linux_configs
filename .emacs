(require 'package) ;; load the package manager
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/addons/")

;; Activate package autoloads
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

;;Display an overlay in each window showing a unique key. In the mean time, ask user for the window where move to
(require 'switch-window)
(global-set-key (kbd "C-e") 'switch-window)

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

;;;; ODT back-end
(eval-after-load "org" '(require 'ox-odt nil t))

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
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(add-to-list 'tramp-remote-path "/usr/local/R/R-3.5.1/lib/R/bin/")

(require 'ess-site)
(ess-toggle-S-assign nil)
(global-set-key (kbd "M--")  (lambda () (interactive) (insert " <- ")))
(setq ess-default-style 'RStudio)
(ess-toggle-underscore nil)             ; disable 'smart underscore'

(setq comint-scroll-to-bottom-on-input t) ;;;; Scroll down when R generates output
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(define-key ess-mode-map (kbd "<M-return>") 'ess-eval-region)
(define-key ess-mode-map (kbd "<C-return>") 'ess-eval-line-and-step)
(define-key ess-mode-map (kbd "<C-S-return>") 'rmd-send-chunk)
(define-key ess-mode-map (kbd "C-S-i") 'rmd-new-chunk)
(define-key ess-mode-map (kbd "C-S-k") 'dev-off)
(define-key poly-markdown+r-mode-map (kbd "C-S-i") 'rmd-new-chunk)

(define-key ess-mode-map (kbd "C-e") 'switch-window)
(define-key inferior-ess-mode-map (kbd "C-e") 'switch-window)

;;;; Remote sessions

(defvar R-remote-host "ada")
(defvar R-remote-dtach-directory "/d0-mendel/home/viktor_petukhov/.dtach/")
(defvar R-remote-directory "/d0-mendel/home/viktor_petukhov/Copenhagen/NeuronalMaturation/")
(defvar R-remote-session "nm")
(defun R-remote (&optional remote-host session directory)
  "Connect to the remote-host's dtach session running R."
  (interactive (list
                (read-from-minibuffer "R remote host: " R-remote-host)
                (read-from-minibuffer "R remote session: " R-remote-session)
                (read-from-minibuffer "R remote directory: " R-remote-directory)))
  (pop-to-buffer (make-comint (concat "remote-" session)
                              "ssh" nil "-C" "-t" remote-host
                              "source ~/.bashrc; cd" directory ";"
                              "dtach" "-A" (concat R-remote-dtach-directory ".dtach-" session)
                              "-z" "-E" "-r" "none"
                              inferior-R-program-name "--no-readline"
                              inferior-R-args))
  (ess-remote (process-name (get-buffer-process (current-buffer))) "R")
  (setq comint-process-echoes t))

;; ;; Company
;; ;; provides popup autocompletion, particularly for R
;; (require 'company)
;; (add-hook 'after-init-hook 'global-company-mode)

;; (define-key inferior-ess-mode-map (kbd "TAB") 'company-complete)
;; (define-key ess-mode-map (kbd "TAB") 'company-complete)

;; ;;;; company-quickhelp
;; ;;;; popups with documentation
;; (company-quickhelp-mode)

;; put cursor in last used position when re-opening file
(require 'saveplace)
(setq-default save-place t)

;; Code navigation
(require 'imenu)
(setq imenu-auto-rescan t) ;; automatically update functions list in the buffer
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

(setq-default cursor-type 'bar) ;; thin cursor

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

(defun dev-off()
  (interactive)
  (ess-eval-linewise "dev.off()"))

;; Spell-check for two languages
(defvar lcl-var:spelling-ignore nil)

(defun lcl:spelling-add-to-dictionary (marked-text)
  (let* ((word (downcase (aref marked-text 0)))
         (dict (if (string-match "[a-zA-Z]" word)
                   (message "en_US.dic")
                 (message "ru_RU.dic")))
         (file (concat "~/.config/enchant/" dict)))
    (when (and file (file-writable-p file))
      (with-temp-buffer
        (insert word) (newline)
        (append-to-file (point-min) (point-max) file)
        (message "Added word \"%s\" to the \"%s\" dictionary" word dict))
      (wcheck-mode 0)
      (wcheck-mode 1))))

(defun lcl:spelling-add-to-ignore (marked-text)
  (let ((word (aref marked-text 0)))
    (add-to-list 'lcl-var:spelling-ignore word)
    (message "Added word \"%s\" to the ignore list" word)
    (wcheck--hook-outline-view-change)))

(defun lcl:spelling-action-menu (marked-text)
  (append (wcheck-parser-ispell-suggestions)
          (list (cons "[Add to dictionary]" 'lcl:spelling-add-to-dictionary)
                (cons "[Ignore]" 'lcl:spelling-add-to-ignore))))

(defun lcl:delete-list (delete-list list)
  (dolist (el delete-list)
    (setq list (remove el list)))
  list)

(defun lcl:spelling-parser-lines (&rest ignored)
  (lcl:delete-list lcl-var:spelling-ignore
                   (delete-dups
                    (split-string
                     (buffer-substring-no-properties (point-min) (point-max))
                     "\n+" t))))

(defun cfg:spelling ()
  (require 'wcheck-mode)
  (defun wcheck--choose-action-minibuffer (actions)
    (cdr
     (assoc
      (ido-completing-read "Choose " (mapcar #'car actions))
      actions)))
  (setq-default
   wcheck-language "All"
   wcheck-language-data
   '(("All"
      (program . "/home/viktor/local/bin/spell_check_text.sh")
      (parser . lcl:spelling-parser-lines)
      (action-program . "/home/viktor/local/bin/spell_check_word.sh")
      (action-parser . lcl:spelling-action-menu)
      (read-or-skip-faces
       ((emacs-lisp-mode c-mode c++-mode python-mode)
        read font-lock-comment-face)
       (org-mode
        skip org-block-begin-line org-block-end-line org-meta-line org-link)
       (nil))
      ))))
(cfg:spelling)

(global-set-key (kbd "C-P") 'wcheck-actions)
(add-hook 'org-mode-hook 'wcheck-mode) ;; Run wcheck with org-mode

;; Final rebindings
(global-set-key (kbd "<end>") 'move-end-of-line)
