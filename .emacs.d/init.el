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
  (global-set-key (kbd "C-c r") 'avy-goto-char)
  (global-set-key (kbd "C-c s") 'avy-goto-char-2)
  ;; set the keys to use as targets to dvorak home row
  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

;;(package-initialize)
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
    (cider clojure-mode projectile better-defaults markdown-preview-mode go-mode))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "light steel blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "orange red")))))
