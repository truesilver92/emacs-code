(setq debug-on-error t)
;; init the package manager
(require 'package)

(add-to-list 'package-archives
	     '("melpa-unstable" . "http://melpa.org/packages/")
	     '("melpa-stable" . "http://melpa-stable.org/packages/"))

(setq geiser-active-implementations '(guile))
(package-initialize)

; gets rid of the blinking cursor because I hate it
(blink-cursor-mode 0)

(desktop-save-mode 1)
(setq desktop-restore-eager 10)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray15" :foreground "light steel blue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(cursor ((t (:background "orange red")))))
