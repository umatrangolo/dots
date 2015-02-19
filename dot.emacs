(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(setq user-full-name "Ugo Matrangolo")
(setq user-mail-address "ugo.matrangolo@gmail.com")

;; switch off all pointless stuff
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; skip the splash screen and go straight to *scratch*
(setq inhibit-splash-screen t
      initial-scratch-message nil)

(when window-system
  (global-linum-mode 1)	;; enable global linum mode
  (global-hl-line-mode 1) ;; hilight current line
  (set-face-background 'hl-line "yellow")
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-font 'font-lock-keyword-face "-*-Source Code Pro-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 110
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "Source Code Pro"
                                 :width 'normal
                                 :size 11
                                 :weight 'normal))))


;; best fit for a Mac Book Pro Retina screen
(when window-system (set-frame-size (selected-frame) 170 82))

;; no tabs, only 2 spaces for indentation
(setq tab-width 2 indent-tabs-mode nil)

;; no backup files please
(setq make-backup-files nil)

;; highlight parens
(show-paren-mode t)

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-scala-executable "/Applications/scala-2.11.4/bin/scalac")

;; remove trialing whitespaces on saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use always spaces
(setq-default indent-tabs-mode nil)

;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("07010c84f8df191b94cd508ad977a9dad43f685f6ba962f4134e2d130c45aa44" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "46fd293ff6e2f6b74a5edf1063c32f2a758ec24a5f63d13b07a20255c074d399" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" default)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
