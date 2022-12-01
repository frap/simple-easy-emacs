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
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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

(use-package emacs
  :config
  (load-theme 'modus-vivendi)
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
	initial-scratch-message "")
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
  :bind ("<f5>" . modus-themes-toggle))

(use-package mood-line
  :straight (:host github :repo "benjamin-asdf/mood-line")
  :config
  (setf mood-line-show-cursor-point t)
  (mood-line-mode))

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

;; Editor Functions
(cond ((eq system-type 'darwin)
       (setq  mac-command-modifier        'super
              mac-option-modifier         'meta
              mac-right-option-modifier   'alt
              mac-pass-control-to-system   nil)))

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

;; Coding Setup

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

(use-package restclient)

(use-package js2-mode
  :ensure t
  :init
  (setq js-basic-indent 2)
  (setq-default js2-basic-indent 2
                js2-basic-offset 2
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
	((dir (expand-file-name "simple-easy-clojure-hello" (temporary-file-directory)))
	 (_ (make-directory dir t))
	 (default-directory dir))
      (shell-command "echo '{}' > deps.edn")
      (make-directory "src" t)
      (find-file "src/hello.clj")
      (when (eq (point-min) (point-max))
	(insert "(ns hello)\n\n(defn main []\n  (println \"hello world\"))\n\n\n;; this is a Rich comment, use it to try out pieces of code while you develop.\n(comment\n  (def rand-num (rand-int 10))\n  (println \"Here is the secret number: \" rand-num))"))
      (call-interactively #'cider-jack-in-clj))))
