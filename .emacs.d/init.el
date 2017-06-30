;; set character encoding
(set-language-environment "UTF-8")
(setq debug-on-error t)
;; init the package manager
(require 'package)

(add-to-list 'package-archives
;	     '("melpa-unstable" . "http://melpa.org/packages/")
	     '("marmalade" . "https://marmalade-repo.org/packages/")
	     '("melpa-stable" . "http://melpa-stable.org/packages/"))
;;(add-to-list 'load-path "/home/joel/emacs-code/.emacs.d/go-mode.el/go-mode.el")
;;(add-to-list 'load-path "/home/joel/emacs-code/.emacs.d/go-mode-autoloads.el")

;; auto install of packages I want
(defvar my-packages '(better-defaults
                      projectile
                      clojure-mode
                      cider
                      paredit
                      geiser))

(package-initialize) ; so package management works

(package-refresh-contents)

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;;add all files in the git directory to the load-path
(let ((default-directory  "~/projects/emacs-code/.emacs.d/git"))
  (normal-top-level-add-subdirs-to-load-path))

;;load the project in the git subdirectory changing some default behavior of emacs
(require 'better-defaults)

(setq geiser-active-implementations '(guile))
(package-initialize)
;;(require 'go-mode-autoloads)

; gets rid of the blinking cursor because I hate it
(blink-cursor-mode 0)
;; I also hate it in terminal mode
(setq visible-cursor nil)

;; multiple spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

;;(desktop-save-mode 1)
;;(setq desktop-restore-eager 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (misterioso)))
 '(desktop-path (quote (".")))
 '(package-selected-packages
   (quote
    (cider clojure-mode projectile better-defaults markdown-preview-mode go-mode geiser))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "light steel blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "orange red")))))
