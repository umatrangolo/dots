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
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 120
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "Source Code Pro"
                                 :width 'normal
                                 :size 12
                                 :weight 'normal))))


;; best fit for a Mac Book Pro Retina screen
(when window-system (set-frame-size (selected-frame) 170 76))

;; no tabs, only 2 spaces for indentation
(setq tab-width 2 indent-tabs-mode nil)

;; no backup files please
(setq make-backup-files nil)

;; highlight parens
(show-paren-mode t)

;; enable flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-scala-executable "/Applications/scala-2.11.4/bin/scalac")

;;
