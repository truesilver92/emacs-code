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
