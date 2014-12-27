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
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x
(helm-mode 1)

