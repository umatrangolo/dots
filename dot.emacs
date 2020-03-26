(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(exec-path-from-shell-initialize)
(setenv "GOPATH" "/Users/umatrangolo/Development/golang")
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOPATH") "/bin"))

;; if you want to change prefix for lsp-mode keybindings.
(setq lsp-keymap-prefix "s-l")

(require 'lsp-mode)
(add-hook 'prog-mode-hook #'lsp)

;; This is only needed once, near the top of the file
(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(setq user-full-name "Ugo Matrangolo")
(setq user-mail-address "ugo.matrangolo@gmail.com")

(when window-system
  (global-linum-mode 1)	;; enable global linum mode
  (global-hl-line-mode 1) ;; hilight current line
  (set-face-background 'hl-line "yellow")
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tool-bar-mode -1)
  )

;; best fit for a Mac Book Pro Retina screen
(when window-system (set-frame-size (selected-frame) 170 78))

;; no tabs, only 2 spaces for indentation
(setq tab-width 2 indent-tabs-mode nil)

;; no backup files please
(setq make-backup-files nil)

;; highlight parens
(show-paren-mode t)

; remove trailing whitespaces on saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use always spaces
(setq-default indent-tabs-mode nil)

;; do not truncate lines
(setq-default truncate-lines 1)

;; force UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(when window-system
  (scroll-bar-mode -1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell go-mode company-lsp flycheck lsp-ui lsp-mode ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
