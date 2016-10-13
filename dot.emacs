(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq user-full-name "Ugo Matrangolo")
(setq user-mail-address "ugo.matrangolo@gmail.com")

;; switch off all pointless stuff
(tool-bar-mode -1)

(when window-system
  (global-linum-mode 1)	;; enable global linum mode
  (global-hl-line-mode 1) ;; hilight current line
  (set-face-background 'hl-line "yellow")
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
)

;; best fit for a Mac Book Pro Retina screen
(when window-system (set-frame-size (selected-frame) 170 77))

;; no tabs, only 2 spaces for indentation
(setq tab-width 2 indent-tabs-mode nil)

;; no backup files please
(setq make-backup-files nil)

;; highlight parens
(show-paren-mode t)

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-scala-executable "/Applications/scala-2.11.7/bin/scalac")

;; remove trailing whitespaces on saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use always spaces
(setq-default indent-tabs-mode nil)

;; require Helm
(require 'helm-config)

;; Auto udpate packages
(auto-package-update-maybe)

;; set GOPATH
(setenv "GOPATH" "/Users/umatrangolo/Development/golang")

;; Autocompletion for GO
(require 'auto-complete-config)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

(defun auto-complete-for-go ()
(auto-complete-mode 1))
 (add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; Font
(setq source-code-pro-normal-12 "-*-Source Code Pro-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(setq source-code-pro-normal-11 "-*-Source Code Pro-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1")
(setq source-code-bold-normal-12 "-*-Source Code Pro-semibold-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(set-face-attribute 'default nil :font source-code-pro-normal-12)

;; Customize scala-mode2
(when window-system
  (scroll-bar-mode -1)
  (add-hook 'scala-mode-hook (lambda ()
                               ;; (set-face-font 'scala-font-lock:override-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:override-face "blue")
                               ;; (set-face-font 'scala-font-lock:private-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:private-face "blue")
                               ;; (set-face-font 'scala-font-lock:abstract-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:abstract-face "blue")
                               ;; (set-face-font 'scala-font-lock:protected-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:protected-face "blue")
                               ;; (set-face-font 'scala-font-lock:implicit-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:implicit-face "blue")
                               ;; (set-face-font 'scala-font-lock:lazy-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:lazy-face "blue")
                               ;; (set-face-font 'scala-font-lock:font-lock-keyword-face source-code-bold-normal-12)
                               ;; (set-face-foreground 'scala-font-lock:lazy-face "blue")
                               )))

;; font-lock-keyword-face

;; do not truncate lines
(setq-default truncate-lines 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(flycheck-go-build-executable "/usr/local/go/bin/go")
 '(package-selected-packages
   (quote
    (go-autocomplete auto-package-update auto-complete yaml-mode typescript-mode typescript sql-indent solarized-theme scala-mode2 sbt-mode rich-minority powerline php-mode markdown-mode+ magit js3-mode icicles helm-projectile go-projectile flyspell-lazy flycheck doremi angular-mode ag)))
 '(projectile-global-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "Firebrick" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:implicit-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:override-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:private-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:sealed-face ((t (:foreground "RoyalBlue1" :weight bold)))))
