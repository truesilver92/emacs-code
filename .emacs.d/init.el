; init the package manager
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
)

; gets rid of the blinking cursor because I hate it
(blink-cursor-mode 0)

; commands for making c/c++ editing better
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
;(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

; major minor mode hooks
; 'prog-mode-hook runs during any programming mode
(add-hook 'prog-mode-hook #'electric-pair-mode)

; add {} to the list of characters electric-pair handles
(add-hook 'prog-mode-hook
		  (lambda ()
			(define-key prog-mode-map "{" 'electric-pair)))

;; windows open when you close are opened on next open
(desktop-save-mode t)
;; prevents emacs from becoming slow from having a lot of files to load from desktop-save-mode
(setq desktop-restore-eager 10)

;; helm and async setup
(add-to-list 'load-path "/home/joel/.emacs.d/helm")
(add-to-list 'load-path "/home/joel/.emacs.d/emacs-async")
(require 'helm-config)

(when (require 'dired-aux)
  (require 'dired-async))

;; helm implement
(global-set-key [?\M-x] 'helm-M-x)
(helm-mode 1)

