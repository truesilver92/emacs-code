
;; add melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(eval-when-compile
  (require 'use-package))

(use-package better-defaults)
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'info
    (info-initialize)
    (add-to-list 'Info-directory-list
		 "~/.emacs.d/git/magit/Documentation/")))
(use-package exec-path-from-shell
  :config
  (let ()
    (exec-path-from-shell-copy-env "SSH_AGENT_PID")
    (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")))
(use-package cider)
(use-package clojure-mode)
(use-package paredit
  :hook ((emacs-lisp-mode
	   eval-expression-minibuffer-setup
	   ielm-mode
	   lisp-mode
	   lisp-interaction-mode
	   scheme-mode) . paredit-mode))
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode +1))
(use-package avy
  :bind (("C-c s" . avy-goto-char)
	 ("C-c r" . avy-goto-char-2))
  :config
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))
(use-package company
  :init (global-company-mode)
  :bind ("M-/" . company-complete))
(use-package org-bullets
  :hook (org-mode . (lambda ()
		      (org-bullets-mode 1)
		      (auto-fill-mode))))


;; set character encoding
(set-language-environment "UTF-8")

(setq column-number-mode t)

;; set the fontsize
(set-face-attribute 'default nil :height 180)

;; turn off menu
(add-hook 'after-init-hook (lambda () (menu-bar-mode 0)))

;; let more than one emacsclient frame have the same buffer open
(setq ido-default-buffer-method 'selected-window)

(setq debug-on-error t)

;;add all files in the git directory to the load-path
;(let ((default-directory  "~/projects/emacs-code/.emacs.d/git"))
;  (normal-top-level-add-subdirs-to-load-path))

;(require 'geiser)

(require 'yaml-mode)
(progn
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(require 'php-mode)
(progn
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

(require 'graphql-mode)

(require 'csharp-mode)
(progn
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode)))

(require 'julia-mode)
(progn
  (add-to-list 'auto-mode-alist '("\\.jl\\'" . julia-mode)))

(require 'markdown-mode)
(progn
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (setq markdown-command "pandoc"))

(require 'elixir-mode)
(progn
  (add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))
  (add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode)))

(progn ; settings for cc mode (c/c++...)
  (setq c-default-style "linux" ; change indentation style
        c-basic-offset 2))

;;(require 'haskell-mode)
;;(progn
  ;;(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
  ;;(add-to-list 'load-path "~/.emacs.d/git/haskell-mode/")
  ;;(require 'haskell-mode)
  ;;(add-to-list 'Info-default-directory-list "~/.emacs.d/git/haskell-mode/"))

;;(package-initialize)
;;(require 'go-mode-autoloads)

; gets rid of the blinking cursor because I hate it
(blink-cursor-mode 0)
;; I also hate it in terminal mode
(setq visible-cursor nil)

;; multiple spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

;; theme loading

;; makes all themes trusted
(setq custom-safe-themes t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/ample-zen/")
(load-theme 'ample-zen t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/clues/")
;;(load-theme 'clues t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk/")
;;(load-theme 'cyberpunk t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/grandshell/")
;;(load-theme 'grandshell t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/soothe/")
;;(load-theme 'soothe t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/monokai/")
;;(load-theme 'monokai t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends (quote (ascii beamer html icalendar latex md odt org)))
 '(package-selected-packages
   (quote
    (use-package projectile php-mode paredit markdown-preview-mode go-mode geiser cider better-defaults))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
