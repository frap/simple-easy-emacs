;; Disable package.el in favour of straight.el
(setq package-enable-at-startup nil)

;;startup message
(message "Chargement en cours early-init.el...")

;; Garbage collection slows down startup time, so we maximise the threshold for
;; it to run, and we will later reset it.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      site-run-file nil
      read-process-output-max (* 10 1024 1024)
      bidi-inhibit-bpa t)

(defvar gas/gc-cons-threshold (* 100 1024 1024))   ;; 100mb

(add-hook 'emacs-startup-hook
          #'(lambda () (setq gc-cons-threshold gas/gc-cons-threshold
                             gc-cons-percentage 0.1)))

;; file-name-handler-alist is consulted on various I/O functions such as
;; REQUIRE, slowing down startup time, so we set it to NIL, and establish a hook
;; to restore when Emacs is finished starting.
;; (unless (or (daemonp) noninteractive)
;;   (let ((file-name-handler-alist/old file-name-handler-alist))
;;     (setq file-name-handler-alist nil)
;;     (add-hook 'emacs-startup-hook
;;               (lambda ()
;;                 (let ((value (delete-dups
;;                               (append file-name-handler-alist
;;                                       file-name-handler-alist/old))))
;;                   (setq file-name-handler-alist value))))))

(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook 'window-setup-hook
            (lambda ()
              (advice-remove #'tty-run-terminal-initialization #'ignore)
              (tty-run-terminal-initialization (selected-frame) nil t))))


(setq-default
 initial-frame-alist '((width . 170)
                       (height . 56)
                       (tool-bar-lines . 0)
                       (vertical-scroll-bars . 0)
                       (bottom-divider-width . 0)
                       (right-divider-width . 1)
                       (font . "Iosevka Curly 13")
                       )
 default-frame-alist initial-frame-alist
 frame-inhibit-implied-resize t            ;; dont resize
 frame-resize-pixelwise t                 ;; as GUI use pixels
 x-gtk-resize-child-frames 'resize-mode
 fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(setq truncate-partial-width-windows nil)

;; Byte compile warnings are useless for newcomers
;; TODO: straight still ends up loggin the warnings
(setq byte-compile-warnings
      '(not
	free-vars
	unresolved
	callargs
	redefine
	obsolete
	noruntime
	interactive-only
	lexical
	lexical-dynamic
	make-local
	mapcar
	not-unused
	constants
	docstrings
	docstrings-non-ascii-quotes
	suspicious))
