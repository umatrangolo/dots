(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

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

;; Font
(setq source-code-pro-normal-12 "-*-Source Code Pro-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(setq source-code-bold-normal-12 "-*-Source Code Pro-semibold-normal-normal-*-12-*-*-*-m-0-iso10646-1")

(set-face-attribute 'default nil :font source-code-pro-normal-12)

;; Customize scala-mode2
(when window-system
  (scroll-bar-mode -1))

;; do not truncate lines
(setq-default truncate-lines 1)

;; force UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; enable flycheck
(global-flycheck-mode)
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-scala-executable "/Applications/scala/bin/scalac")
(setq flycheck-go-build-executable "/usr/local/go/bin/go")
(setenv "GOPATH" "/Users/umatrangolo/Development/golang")

;; Org Mode setup
(setq org-todo-keywords
  '((sequence "TODO" "WIP" "|" "CANCELED" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WIP" . "yellow")
        ("CANCELED" . (:foreground "blue" :weight bold))))

;; Projectile setup
(projectile-mode +1)
(setq projectile-project-search-path '("~/Development"))
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flymake-json json-mode ag nodejs-repl s markdown-preview-mode yaml-mode go-projectile projectile markdown-mode+ flycheck auto-complete go-mode js3-mode magit sbt-mode scala-mode ##))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "Firebrick" :slant italic))))
 '(font-lock-keyword-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:abstract-face ((t (:inherit font-lock-builtin-face :foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:final-face ((t (:inherit font-lock-builtin-face :foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:implicit-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:lazy-face ((t (:inherit font-lock-builtin-face :foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:override-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:private-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:protected-face ((t (:foreground "RoyalBlue1" :weight bold))))
 '(scala-font-lock:sealed-face ((t (:foreground "RoyalBlue1" :weight bold)))))
