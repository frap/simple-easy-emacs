;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun gas/display-startup-time ()
  (message "Emacs chargé dans %s avec %d ramasse-miettes."
           (format "%.2f secondes"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'gas/display-startup-time)

(defconst XDG_CACHE/ (or (getenv "XDG_CACHE_HOME") "~/.cache/"))

(defconst EMACS_CACHE/
  (expand-file-name "emacs/" XDG_CACHE/ )
  "Directory for Emacs volatile storage.
   Use this for files that change often.")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" EMACS_CACHE/ ))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setf
 straight-vc-git-default-protocol 'https
 straight-use-package-by-default t
 use-package-verbose t
 use-package-always-demand t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

;; BETTER DEFAULTS
(use-package emacs
  :config
  (load-theme 'modus-operandi)
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  (setq inhibit-startup-message t)

  (scroll-bar-mode -1)			; Disable visible scrollbar
  (tool-bar-mode -1)			; Disable the toolbar
  (tooltip-mode -1)			; Disable tooltips
  (set-fringe-mode 8)
  (menu-bar-mode t)			; Disable the menu bar
  (setq visible-bell t)
  (blink-cursor-mode -1)
  (column-number-mode)
  ;; (setf display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)

  ;; Disable line numbers for some modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  (setq use-dialog-box nil)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq create-lockfiles nil)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq inhibit-startup-message t
	initial-scratch-message ";; ABANDONNEZ TOUT ESPOIR VOUS QUI ENTREZ ICI\n\n"
	initial-major-mode #'emacs-lisp-mode)
  
;;; Show matching parenthesis
  (show-paren-mode 1)
;;; By default, there’s a small delay before showing a matching parenthesis. Set
;;; it to 0 to deactivate.
  (setq show-paren-delay 0)
  (setq show-paren-when-point-inside-paren t)

  (setq show-paren-style 'parenthesis)
;;; Electric Pairs to auto-complete () [] {} "" etc. It works on regions.

  (electric-pair-mode)
  (setq redisplay-skip-fontification-on-input t
	fast-but-imprecise-scrolling t)
  (global-so-long-mode 1)

  (setq-default
   frame-resize-pixelwise t ; support better certain window managers like ratpoison

   ;; these settings still should be set on a per language basis, this is just a general default
   indent-tabs-mode nil ; spaces > tabs
   tab-width 2 ; tab is 2 spaces
   fill-column 79 ; python friendly

   ;; better security
   gnutls-verify-error t
   gnutls-min-prime-bits 2048

   ;; dont expire a passphrase
   password-cache-expiry nil

   mouse-yank-at-point t
   save-interprogram-paste-before-kill t
   apropos-do-all t
   require-final-newline t
   ediff-window-setup-function 'ediff-setup-windows-plain
   
   ;; the most reliable tramp setup I have found (used at work every day...)
   tramp-default-method "ssh"
   tramp-copy-size-limit nil
   tramp-use-ssh-controlmaster-options nil

   ;; I recommend the following ~/.ssh/config settings be used with the tramp settings in this cfg:
   ;; Host *
   ;; ForwardAgent yes
   ;; AddKeysToAgent yes
   ;; ControlMaster auto
   ;; ControlPath ~/.ssh/master-%r@%h:%p
   ;; ControlPersist yes
   ;; ServerAliveInterval 10
   ;; ServerAliveCountMax 10

   vc-follow-symlinks t ; open symlinks, don't ask confusing questions

   ring-bell-function 'ignore ; be quiet
   )

  (setq backup-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
	`((".*" ,temporary-file-directory t)))
  :bind ("<f5>" . modus-themes-toggle))

;; UNBIND ANNOYANCES

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "M-m"))

;; TAB AUTO COMPLETION
(setq tab-always-indent 'complete) ; used by eglot for auto-completion as well

;; BETTER-DEFAULTS

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)

;; MATCHING BRACKET LIKE VIM's "%"

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(global-set-key (kbd "C-%") 'forward-or-backward-sexp)

;; COLLECTION OF REDICULOUSLY USEFUL EXTENSIONS

(global-set-key (kbd "C-c i") #'(lambda ()
                                  (interactive)
                                  (find-file user-init-file)))

(use-package crux)

(with-eval-after-load 'crux
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
  (global-set-key (kbd "C-o") 'crux-smart-open-line)
  (global-set-key (kbd "C-c C-l") 'crux-duplicate-current-line-or-region)
  (global-set-key (kbd "C-c C--") 'crux-kill-whole-line)
  (global-set-key (kbd "C-c ;") 'crux-duplicate-and-comment-current-line-or-region))

;; BROWSE KILL RING

(with-eval-after-load 'browse-kill-ring
  (global-set-key (kbd "M-y") 'browse-kill-ring))

;; REGEXP SEARCH

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; FIND FILES

(defalias 'ff 'find-name-dired)

;; GREP FILES

(defalias 'rg 'rgrep)

;; DIFF

(defalias 'ed 'ediff)
(defalias 'edb 'ediff-buffers)

;; GIT

(with-eval-after-load 'magit
  (defalias 'git 'magit))

;; LINTER

(with-eval-after-load 'flymake
  (defun spartan-lint ()
    (interactive)
    (flymake-mode 1)
    (flymake-show-diagnostics-buffer))
  (defalias 'lint 'spartan-lint))

;; Explicitly set an Emacs environment as desired.
;; NO MORE editing .bash_profile or whatever or messing with packages like `exec-path-from-shell' !

(require 'subr-x)

;; if EDITOR is not set already, set it.
(or (getenv "EDITOR")
    (progn
      (setenv "EDITOR" "emacsclient")
      (setenv "VISUAL" (getenv "EDITOR"))))

;; if PAGER is not set already, set it
(or (getenv "PAGER")
    (setenv "PAGER" "cat"))


;; 'PATH' modifications

(setq spartan-path-insert '(
                            "~/bin"
                            "~/.local/bin"
                            ))

(setq spartan-path-append '(
                            ;; "~/Put/To/End/Of/PATH"
                            ))

;; Help out MacOS users to make dev env more like-linux
;; HINT: brew install coreutils findutils gnu-tar gnu-sed gawk gnutls gnu-indent gnu-getopt grep bash

(when (file-directory-p "/opt/homebrew")
  (setq gnubin-locations
        (split-string (shell-command-to-string "find /opt/homebrew -name \"*gnubin*\" -type d") "\n" t))
  (add-to-list 'gnubin-locations "/opt/homebrew/bin" t)
  (add-to-list 'gnubin-locations "/opt/homebrew/sbin" t)

  (dolist (item gnubin-locations)
    (add-to-list 'spartan-path-insert item)))

;; SET matching exec-path and 'PATH' values with inserts/appends

(dolist (item spartan-path-insert)
  (add-to-list 'exec-path item))

(dolist (item spartan-path-append)
  (add-to-list 'exec-path item t))

(setenv "PATH" (string-trim-right (string-join exec-path ":") ":$"))

;; MACOS
(cond ((eq system-type 'darwin)
       (setq  mac-command-modifier        'super
              mac-option-modifier         'meta
              mac-right-option-modifier   'alt
              mac-pass-control-to-system   nil)))

;; UI
(use-package mood-line
  :straight (:host github :repo "benjamin-asdf/mood-line")
  :config
  (setf mood-line-show-cursor-point t)
  (mood-line-mode))

(use-package all-the-icons
  :if (display-graphic-p)
  :after (marginalia dired)
  :init
  (add-hook 'marginalia-mode #'all-the-icons-completion-marginalia-setup)
  (add-hook 'dired-mode #'all-the-icons-dired-mode)
  :config
  (use-package all-the-icons-dired)
  (use-package all-the-icons-completion)
  :config
  (all-the-icons-completion-mode 1))

(let ((font "Iosevka Term Curly Medium 13"))
  (add-to-list 'default-frame-alist `(font . ,font))
  (set-face-attribute 'default t :font font :height 130)
  (set-face-attribute 'default nil :font font :height 130)
  (set-frame-font font nil t))

(use-package default-text-scale
  :bind (( "M--" . default-text-scale-decrease)
         ( "M-+" . default-text-scale-increase)
         ( "M-=" . default-text-scale-reset))
  :config
  (setq default-text-scale-mode 1))

;; Dimm the colours of inactive windows
(use-package dimmer
  :config
  (setq dimmer-fraction 0.3)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorsapce :rgb)
  (dimmer-mode 1))


;; NAV
(use-package vertico
  :init
  (vertico-mode)
  (setq vertico-scroll-margin 0)
  (setq vertico-count 20)
  (setq vertico-cycle t))

 ;; save-place remembers the position of pt in a buffer and on re-opening restores it
  (use-package saveplace
    :straight nil
    :config (save-place-mode 1))
  ;; savehist same as saveplace but for history
  (use-package savehist
    :straight nil
    :custom (savehist-file (concat EMACS_CACHE/ "savehist"))
    :config
     (setq savehist-save-minibuffer-history t
	  savehist-autosave-interval nil     ; save on kill only
	  savehist-additional-variables
	  '(kill-ring                        ; persist clipboard
	    register-alist                   ; persist macros
	    mark-ring global-mark-ring       ; persist marks
	    search-ring regexp-search-ring)) ; persist searches
    (add-hook 'savehist-save-hook
      (defun doom-savehist-unpropertize-variables-h ()
	"Remove text properties from `kill-ring' to reduce savehist cache size."
	(setq kill-ring
	      (mapcar #'substring-no-properties
		      (cl-remove-if-not #'stringp kill-ring))
	      register-alist
	      (cl-loop for (reg . item) in register-alist
		       if (stringp item)
		       collect (cons reg (substring-no-properties item))
		       else collect (cons reg item))))
      (defun doom-savehist-remove-unprintable-registers-h ()
	"Remove unwriteable registers (e.g. containing window configurations).
  Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
  the unwritable tidbits."
	;; Save new value in the temp buffer savehist is running
	;; `savehist-save-hook' in. We don't want to actually remove the
	;; unserializable registers in the current session!
	(setq-local register-alist
		    (cl-remove-if-not #'savehist-printable register-alist)))))

(use-package orderless
  :init
  (setq
   completion-styles '(orderless)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

(require 'dired)
;; better dired
(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))

;; https://github.com/Gavinok/emacs.d
(use-package consult
  :bind (("C-x b"       . consult-buffer)
         ("C-x C-k C-k" . consult-kmacro)
         ("M-y"         . consult-yank-pop)
         ("M-g g"       . consult-goto-line)
         ("M-g M-g"     . consult-goto-line)
         ("M-g f"       . consult-flymake)
         ("M-g i"       . consult-imenu)
         ("M-s l"       . consult-line)
         ("M-s L"       . consult-line-multi)
         ("M-s u"       . consult-focus-lines)
         ("M-s g"       . consult-grep)
         ("M-s M-g"     . consult-grep)
         ("C-x C-SPC"   . consult-global-mark)
         ("C-x M-:"     . consult-complex-command)
         ("C-c n"       . consult-org-agenda)
         :map dired-mode-map
         ("O" . consult-file-externally)
         :map help-map
         ("a" . consult-apropos)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :config
  (recentf-mode t))


;; EDITOR
(use-package form-feed
  :config (global-form-feed-mode))

(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :mode "\\.md\\'"
  :hook ((markdown-mode . auto-fill-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ob-restclient)

 (use-package org
    :straight (:type built-in)
    :diminish t
    :preface
    ;; Set my default org-export backends. This variable needs to be set before
    ;; org.el is loaded.
    (setq org-export-backends '(ascii html latex md))
    ;; Do not open links of mouse left clicks.
    ;; Default behavior caused inline images in Org buffers to pop up in their
    ;; own buffers when left clicked on by mistake. I can still intentionally
    ;; open links and such images in new buffers by doing C-c C-o.
    (setq org-mouse-1-follows-link nil)
    :bind (("C-c c" . org-capture)
	   ("C-c a" . org-agenda)
	   ("<f7> s" . org-store-link)
	   :map org-src-mode-map
	   ("C-x w" . org-edit-src-exit)
	   ("C-x C-s" . org-edit-src-exit))
    :mode ("\\.org\\'" . org-mode)
    :custom-face
    (org-block ((t (:extend t))))
    (org-block-begin-line ((t ( :slant unspecified
				:weight normal
				:background unspecified
				:inherit org-block
				:extend t))))
    (org-block-end-line ((t ( :slant unspecified
			      :weight normal
			      :background unspecified
			      :inherit org-block-begin-line
			      :extend t))))
    (org-drawer ((t (:foreground nil :inherit shadow))))
    :custom
    (org-ellipsis "…")
    :init
    ;; This is where my ~heart~ org files are.
    (setq org-directory "~/logseq")
    :config
    (require 'ob-restclient)
    ;;;; general settings
    (setq org-adapt-indentation nil)      ; No, non, nein, όχι!
    ;; Prevent auto insertion of blank lines before headings and list items
    (setq org-blank-before-new-entry '((heading)
				       (plain-list-item)))
    (setq org-structure-template-alist    ; CHANGED in Org 9.3, Emacs 27.1
	  '(("s" . "src")
	    ("E" . "src emacs-lisp")
	    ("e" . "example")
	    ("q" . "quote")
	    ("v" . "verse")
	    ("V" . "verbatim")
	    ("c" . "center")
	    ("C" . "comment")))
    (setq org-catch-invisible-edits 'smart) ;; try not to accidently do wierd stuff in invisible regions : show smart, error
    (setq org-return-follows-link t)

     ;; Allow _ and ^ characters to sub/super-script strings but only when
    ;; string is wrapped in braces
    (setq org-use-sub-superscripts '{}) ; In-buffer rendering
    (setq org-pretty-entities t)        ; special symbols, latex
    ;; Render subscripts and superscripts in Org buffers
    (setq org-pretty-entities-include-sub-superscripts t)
    (setq org-insert-heading-respect-content t)

    ;;;; code blocks
    (setq org-hide-block-startup nil)
    (setq org-fontify-quote-and-verse-blocks t
	  org-fontify-whole-heading-line t)
    (setq org-confirm-babel-evaluate nil)
    (setq org-src-window-setup 'current-window)
    (setq org-edit-src-persistent-message nil)
    (setq org-src-fontify-natively t) ;; Display entities like \tilde, \alpha, etc in UTF-8 characters
    (setq org-src-preserve-indentation t)
    (setq org-src-tab-acts-natively t) ;; TAB as if code tab settings
    (setq org-edit-src-content-indentation 0) ;; remove 2 space indent in src code blocks
    (setq org-use-property-inheritance t) ;; for tangling

    ;; https://orgmode.org/manual/Clean-view.html
    (setq org-startup-indented t)       ;;; removed leading * for nicer view
    (with-eval-after-load 'org-indent
      (setq org-indent-indentation-per-level 1)) ;Default = 2

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (clojure . t)
       (shell . t)
       (R . t)
       (lisp . t)
       (sqlite . t)
       (python . t)
       (latex . t)
       (restclient . t)
       ))
     (define-advice org-return (:around (f &rest args))
      (let ((org-src-preserve-indentation t))
	(apply f args)))
    (define-advice org-cycle (:around (f &rest args))
      (let ((org-src-preserve-indentation t))
	(apply f args)))
    (defun org-babel-edit-prep:emacs-lisp (_)
      "Setup Emacs Lisp buffer for Org Babel."
      (setq lexical-binding t)
      (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))
    (defun gas/org-setup ()
    ;;  (org-indent-mode)       ;; turn on org indent
      (variable-pitch-mode 1) ;; turn on variable-pitch
      (auto-fill-mode +1)      ;; turn on auto-fill
      (visual-line-mode 1)    ;; turn on visual-line-mode
     ;; (show-paren-mode 1)     ;; show parentheses
      (hl-line-mode +1)
      (auto-fill-mode +1)
      (whitespace-mode -1)
      (electric-indent-local-mode -1)
      (setq-local cursor-type 'bar)
      (setq-local delete-trailing-lines t)
      (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)
      )
    )

 (use-package  which-key
    :hook (after-init . which-key-mode)
    :config
    (which-key-mode 1)
    (setq which-key-add-column-padding 2)
    (setq which-key-idle-delay 0.5)
    (setq which-key-idle-secondary-delay 0.1)
    (setq which-key-max-display-columns nil)
    (setq which-key-min-display-lines 6)
    (setq which-key-replacement-alist
          '((("left") . ("🡸"))
            (("right") . ("🡺"))
            (("up") . ("🡹"))
            (("down") . ("🡻"))
            (("delete") . ("DEL"))
            (("\\`DEL\\'") . ("BKSP"))
            (("RET") . ("⏎"))
            (("next") . ("PgDn"))
            (("prior") . ("PgUp"))))
    (setq which-key-sort-order 'which-key-key-order-alpha)
    (setq which-key-sort-uppercase-first nil)
    (which-key-setup-minibuffer)
    ;;  (:with-hook which-key-init-buffer-hook
    ;;  (:hook (lambda (setq line-spacing 4))))
    )

 (use-package recentf
  ;; Keep track of recently opened files
    :straight nil
    :commands recent-open-files
    :custom (recentf-save-file (concat EMACS_CACHE/ "recentf"))
    :config
    (setq recentf-auto-cleanup nil     ; Don't. We'll auto-cleanup on shutdown
	  recentf-max-saved-items 200) ; default is 20

    (defun doom--recentf-file-truename-fn (file)
      (if (or (not (file-remote-p file))
	      (equal "sudo" (file-remote-p file 'method)))
	  (abbreviate-file-name (file-truename (tramp-file-name-localname tfile)))
	file))

    ;; Resolve symlinks, strip out the /sudo:X@ prefix in local tramp paths, and
    ;; abbreviate $HOME ~ in filepaths (more portable, more readable, & saves
    ;; space)
    (add-to-list 'recentf-filename-handlers #'doom--recentf-file-truename-fn)

    ;; Text properties inflate the size of recentf's files, and there is
    ;; no purpose in persisting them (Must be first in the list!)
    (add-to-list 'recentf-filename-handlers #'substring-no-properties))

;; CODING

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode)
  :config
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package bash-completion
  :init
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
            #'bash-completion-dynamic-complete)

  :config
  (defun bash-completion-capf-1 (bol)
    (bash-completion-dynamic-complete-nocomint (funcall bol) (point) t))
  (defun bash-completion-eshell-capf ()
    (bash-completion-capf-1 (lambda () (save-excursion (eshell-bol) (point)))))
  (defun bash-completion-capf ()
    (bash-completion-capf-1 #'point-at-bol))
  (add-hook
   'sh-mode-hook
   (defun mm/add-bash-completion ()
     (add-hook 'completion-at-point-functions #'bash-completion-capf nil t))))

(use-package shell
  :ensure nil
  :config
  (defun mm/with-current-window-buffer (f &rest args)
    (with-current-buffer
	(window-buffer (car (window-list)))
      (apply f args)))

  (defun mm/shell-via-async-shell-command ()
    (switch-to-buffer
     (window-buffer
      (async-shell-command
       shell-file-name))))

  (advice-add #'mm/shell-via-async-shell-command :around #'mm/with-current-window-buffer)

  (setf shell-kill-buffer-on-exit t)

  (add-hook
   'shell-mode-hook
   (defun mm/shell-dont-mess-with-scroll-conservatively ()
     (setq-local scroll-conservatively 0))))

;; (remove-hook 'find-file-hook #'vc-refresh-state)
;; (hook-with-delay 'find-file-hook 1 #'vc-refresh-state)
(use-package magit
      :defer t
      :defines (magit-status-mode-map
        magit-revision-show-gravatars
        magit-display-buffer-function
        magit-diff-refine-hunk)
      :commands (magit-display-buffer-same-window-except-diff-v1
         magit-stage-file
         magit-unstage-file)
      :init
      (setq-default magit-git-executable (executable-find "git"))
      :config
      (setq-default vc-follow-symlinks t)
      ;; properly kill leftover magit buffers on quit
      (define-key magit-status-mode-map
    [remap magit-mode-bury-buffer]
    #'vcs-quit)

      (setq magit-revision-show-gravatars
        '("^Author:     " . "^Commit:     ")
        magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1
        ;; show word-granularity on selected hunk
        magit-diff-refine-hunk t)
      (setq git-commit-summary-max-length 120)
      (setq magit-commit-show-diff nil)
      (setq magit-delete-by-moving-to-trash nil)
      (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1)
      (setq magit-log-auto-more t)
      (setq magit-log-margin-show-committer-date t)
      (setq magit-revert-buffers 'silent)
      (setq magit-save-repository-buffers 'dontask)
      (setq magit-wip-after-apply-mode t)
      (setq magit-wip-after-save-mode t)
      (setq magit-wip-before-change-mode t)
      (setq transient-values
            '((magit-log:magit-log-mode "--graph" "--color" "--decorate"))))

(use-package ediff-wind
      :straight nil
      :defer t
      :init
      (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

(use-package diff-hl
  :defer t
  :hook ((prog-mode . turn-on-diff-hl-mode)
         (text-mode . turn-on-diff-hl-mode)
         (vc-dir-mode . turn-on-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)))

(defun vcs-quit (&optional _kill-buffer)
  "Clean up magit buffers after quitting `magit-status'.
    And don't forget to refresh version control in all buffers of
    current workspace."
  (interactive)
  (quit-window)
  (unless (cdr
           (delq nil
		 (mapcar (lambda (win)
			   (with-selected-window win
			     (eq major-mode 'magit-status-mode)))
			 (window-list))))
    (when (fboundp 'magit-mode-get-buffers)
      (mapc #'vcs--kill-buffer (magit-mode-get-buffers)))))

(defun vcs--kill-buffer (buffer)
  "Gracefully kill `magit' BUFFER.
    If any alive process is related to this BUFFER, wait for 5
    seconds before nuking BUFFER and the process. If it's dead -
    don't wait at all."
  (when (and (bufferp buffer) (buffer-live-p buffer))
    (let ((process (get-buffer-process buffer)))
      (if (not (processp process))
          (kill-buffer buffer)
        (with-current-buffer buffer
          (if (process-live-p process)
              (run-with-timer 5 nil #'vcs--kill-buffer buffer)
            (kill-process process)
            (kill-buffer buffer)))))))

(use-package yasnippet
 ;; :diminish yas-minor-mode
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-snippet-file
             yas-reload-all
             yas-dropdown-prompt
             yas--all-templates
             yas--get-snippet-tables
             yas--template-key)
  :hook ((text-mode . yas-minor-mode-on)
         (prog-mode . yas-minor-mode-on)
         (conf-mode . yas-minor-mode-on)
         (snippet-mode . yas-minor-mode-on))
  :init
  ;;(add-hook 'find-file-hook #'file-templates-check)
  :config
  (setq yas-prompt-functions (delq #'yas-dropdown-prompt
                                   yas-prompt-functions)
       ;; yas-snippet-dirs '(file-templates-dir)
	)
  ;; Ensure file templates in `file-templates-dir' are visible
  (yas-reload-all))

(use-package json-mode
  :ensure t)

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))
  :bind (:map restclient-mode-map
	      ("C-c C-f" . json-mode-beautify)))

(use-package restclient-jq)

(use-package js2-mode
  :ensure t
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-indent-level 2
                js2-auto-indent-p t
                js2-cleanup-whitespace t
                js2-enter-indents-newline t
                js2-indent-on-enter-key t
                js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))

  (add-hook 'js2-mode-hook
            (lambda ()
              (push '("function" . ?ƒ) prettify-symbols-alist)))

  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

(use-package color-identifiers-mode
    :ensure t
    :init
    (add-hook 'js2-mode-hook 'color-identifiers-mode))

(use-package paredit)

;; paredit everywhere
(defun spartan-lisp-hook ()
  (require 'paredit)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook          #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'racket-mode-hook           #'enable-paredit-mode)

  ;; Clojure
  (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs
                   `(clojure-mode . ("clojure-lsp"))))

  (when (executable-find "clojure-lsp")
    (with-eval-after-load 'eglot
      (add-hook 'clojure-mode-hook 'eglot-ensure)))

  (add-hook 'clojure-mode-hook
            (lambda ()
              (define-key clojure-mode-map (kbd "M-m rr") 'inf-clojure-eval-region)
              (define-key clojure-mode-map (kbd "M-m rb") 'inf-clojure-eval-buffer)
              (define-key clojure-mode-map (kbd "M-m rR") 'inf-clojure))))

(add-hook 'after-init-hook 'spartan-lisp-hook)

(use-package cider
  :config
  (setq cider-babashka-parameters "nrepl-server 0"
	clojure-toplevel-inside-comment-form t)

  (cider-register-cljs-repl-type 'nbb-or-scittle-or-joyride "(+ 1 2 3)")

  (defun mm/cider-connected-hook ()
    (when (eq 'nbb-or-scittle-or-joyride cider-cljs-repl-type)
      (setq-local cider-show-error-buffer nil)
      (cider-set-repl-type 'cljs)))
  (add-hook 'cider-connected-hook #'mm/cider-connected-hook)

  ;; silence byte compiler
(require 'clojure-mode)
(require 'cider)

;; Most annoying JVM "feature" of all time
;; https://docs.cider.mx/cider/troubleshooting.html#empty-java-stacktraces
(defun corgi/around-cider-jack-in-global-options (command project-type)
  (if (eq 'clojure-cli project-type)
      (concat cider-clojure-cli-global-options
              " -J-XX:-OmitStackTraceInFastThrow")
    (funcall command project-type)))

(advice-add #'cider-jack-in-global-options :around #'corgi/around-cider-jack-in-global-options)

(defun simple-easy-clojure-hello ()
  (interactive)
  (unless
      (executable-find "clj")
    (user-error
     "Install clojure first! browsing to %s"
     (let ((url "https://clojure.org/guides/install_clojure")) (browse-url url) url)))
  (let*
      ((dir (expand-file-name "simple-easy-clojure-hola" (temporary-file-directory)))
	 (_ (make-directory dir t))
	 (default-directory dir))
      (shell-command "echo '{}' > deps.edn")
      (make-directory "src" t)
      (find-file "src/hola.clj")
      (when (eq (point-min) (point-max))
	(insert "(ns hola)\n\n(defn main []\n  (println \"hola pelotudo\"))\n\n\n;; this is a Rich comment, use it to try out pieces of code while you develop.\n(comment\n  (def rand-num (rand-int 10))\n  (println \"Here is the secret number: \" rand-num))"))
      (call-interactively #'cider-jack-in-clj))))
