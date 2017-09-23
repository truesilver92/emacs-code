;; set character encoding
(set-language-environment "UTF-8")

;; set the fontsize
(set-face-attribute 'default nil :height 110)

;; turn on the menu bar (so it is easier to learn the shortcuts)
(add-hook 'after-init-hook (lambda () (menu-bar-mode 1)))

;; let more than one emacsclient frame have the same buffer open
(setq ido-default-buffer-method 'selected-window)

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
;;(defvar my-packages '(paredit))

;;(package-initialize) ; so package management works

;;(package-refresh-contents)

;;(dolist (p my-packages)
;;  (unless (package-installed-p p)
;;    (package-install p)))

;;add all files in the git directory to the load-path
(let ((default-directory  "~/projects/emacs-code/.emacs.d/git"))
  (normal-top-level-add-subdirs-to-load-path))

;;load the project in the git subdirectory changing some default behavior of emacs
(require 'better-defaults)
;; a git porcelain
(require 'magit)
;; magit stuff
(let ()
 (global-set-key (kbd "C-x g") 'magit-status))

;; magit wanted this I think for its documentation
(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/git/magit/Documentation/"))

(require 'geiser)

;; this makes my ssh-agent work with magit
(require 'exec-path-from-shell)
(let ()
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

(require 'cider)

(require 'clojure-mode)

(require 'paredit)
;; paredit code
(let ()
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

(require 'projectile)

(require 'quickrun)

(require 'avy)
(progn
  ;; set keybindings for the awesome search types
  (global-set-key (kbd "C-c s") 'avy-goto-char)
  (global-set-key (kbd "C-c r") 'avy-goto-char-2)
  ;; set the keys to use as targets to dvorak home row
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(require 'company)
(progn
  (add-hook 'after-init-hook 'global-company-mode)
  (global-set-key (kbd "M-/") 'company-complete))

(require 'org-bullets)
(progn
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (auto-fill-mode))))

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
 '(magit-commit-arguments (quote ("--gpg-sign=joel@ec2software.com"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
